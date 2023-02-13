use super::{Error, External, Module, ModuleSet, Result};
use crate::ast::{self, Dfs as _};
use crate::pattern_matching as p;
use std::collections::HashMap;
use std::rc::Rc;

pub fn run(module: &Module, external: &impl External) -> Result<()> {
    let mut ctx = Context {
        module_set: &(module, external),
        type_finite_cons: HashMap::new(),
        value_con_tags: HashMap::new(),
    };

    for expr in module.ast_root.expressions() {
        expr.dfs(|expr| match expr.rep {
            ast::ExprRep::Match(ref match_) => {
                ctx.validate_match(match_).map_err(|e| e.on(expr.id))
            }
            _ => Ok(()),
        })?;
    }

    // TODO: Here we should validate that all value types can be sized, but this validation is
    // complicated by the inclusion of higher-order type parameters, so we are delaying it
    // until code generation. Here is an example of a program which is difficult to implement validation:
    // (value-data (Foo F)
    //   (foo (Bar F)))
    // (value-data (Bar F)
    //   (bar (F (Foo F))))

    Ok(())
}

struct Context<'a, M> {
    module_set: &'a M,
    type_finite_cons: HashMap<ast::TypeCon, Rc<[p::FiniteCon]>>,
    value_con_tags: HashMap<ast::ValueCon, p::Tag>,
}

impl<'a, M: ModuleSet> Context<'a, M> {
    fn validate_match(&mut self, match_: &ast::ExprMatch) -> Result<()> {
        let rows = match_
            .clauses
            .iter()
            .map(|(p, _)| p::Row::new(vec![self.simplify_pattern(p)].into()))
            .collect::<Vec<_>>();

        if rows.is_empty() {
            // FIXME: In bottom types, it should be considered exhaustive
            return Err(Box::new(Error::NonExhaustivePattern(vec![
                p::Pattern::Wildcard,
            ])));
        }

        p::check(rows).map_err(|e| {
            Box::new(match e {
                p::Error::Useless(mut row) => Error::UselessPattern(row.take_head().unwrap()),
                p::Error::NonExhaustive(mat) => Error::NonExhaustivePattern(
                    mat.into_rows()
                        .into_iter()
                        .map(|mut row| row.take_head().unwrap())
                        .collect(),
                ),
            })
        })
    }

    fn value_cons_of(&mut self, type_con: ast::TypeCon) -> Rc<[p::FiniteCon]> {
        let set = self.module_set;
        Rc::clone(
            self.type_finite_cons
                .entry(type_con)
                .or_insert_with(|| match type_con {
                    ast::TypeCon::Data(id) => {
                        let con = set.ast(id).unwrap();
                        con.value_cons().map(|con| con.into()).collect()
                    }
                    ast::TypeCon::Builtin(id) => {
                        let con = set.ast(id).unwrap();
                        con.value_cons().map(|con| con.into()).collect()
                    }
                }),
        )
    }

    fn tag_of(&mut self, value_con: ast::ValueCon) -> p::Tag {
        if let Some(tag) = self.value_con_tags.get(&value_con) {
            return tag.clone();
        }

        let finite_cons = self.value_cons_of(match value_con {
            ast::ValueCon::Data(id) => self.module_set.ast(id).unwrap().type_con.into(),
            ast::ValueCon::Builtin(id) => self.module_set.ast(id).unwrap().type_con.into(),
        });
        let finite_con = *finite_cons
            .iter()
            .find(|con| con.use_ == value_con)
            .unwrap();
        let tag = p::Tag::Finite(finite_con, finite_cons);

        self.value_con_tags.insert(value_con, tag.clone());
        tag
    }

    fn simplify_pattern(&mut self, pattern: &ast::Pattern) -> p::Pattern {
        match pattern.rep {
            ast::PatternRep::Var(ref var) => match var.as_pat {
                Some(ref p) => self.simplify_pattern(p),
                None => p::Pattern::Wildcard,
            },
            ast::PatternRep::Wildcard => p::Pattern::Wildcard,
            ast::PatternRep::Decon(ref decon) => {
                let ps = decon
                    .fields
                    .iter()
                    .flat_map(|fields| fields.iter())
                    .map(|p| self.simplify_pattern(p))
                    .collect();
                let tag = self.tag_of(*decon.use_.get_resolved());
                p::Pattern::Constructor(tag, ps)
            }
            ast::PatternRep::Const(ref lit) => {
                assert!(
                    !matches!(lit, ast::Const::SyntaxSexp(_)),
                    "Found Const::SyntaxSexp in pattern"
                );
                p::Pattern::Constructor(p::Tag::Const(lit.clone()), Vec::new())
            }
        }
    }
}
