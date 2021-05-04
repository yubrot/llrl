use super::*;
use crate::lexer::Lexer;
use crate::parser::parse;
use crate::sexp::{matcher as m, Sexp, SexpRep};
use crate::source_loc::SourceLocator;

macro_rules! assert_matches {
    ($input:tt, $ty:ty, $($pat:tt)*) => {
        assert!(matches!(
            parse::<Sexp, _>(&mut SourceLocator::temporary(), Lexer::new($input))
                .aggregate_errors()
                .unwrap()
                .matches::<$ty>(),
            $($pat)*
        ));
    };
}

#[test]
fn test_core() {
    assert_matches!(
        "foo",
        Parameterizable<Name, m::Any>,
        Ok(Parameterizable {
            callee: Name { sym: "foo", .. },
            params: None
        })
    );
    assert_matches!(
        "(foo a b)",
        Parameterizable<Name, m::Any>,
        Ok(Parameterizable {
            callee: Name { sym: "foo", .. },
            params: Some([_, _])
        })
    );

    assert_matches!("foo", MacroApply, Err(_));
    assert_matches!(
        "(+ a b)",
        MacroApply,
        Ok(MacroApply {
            callee: Use::Name(Name { sym: "+", .. }),
            args: [_, _],
            ..
        })
    );
}

#[test]
fn test_type() {
    assert_matches!(
        "Int",
        Type,
        Ok(Type::Use(Use::Name(Name { sym: "Int", .. })))
    );
    assert_matches!(
        "(List Int)",
        Type,
        Ok(Type::App(TypeApply { args: [_], .. }))
    );
    assert_matches!(
        "(-> Int)",
        Type,
        Ok(Type::Fun(TypeFun {
            args: [],
            ret: _,
            ..
        }))
    );
}

#[test]
fn test_kind() {
    assert_matches!("*", Kind, Ok(Kind::Type));
    assert_matches!("Constraint", Kind, Ok(Kind::Constraint));
    assert_matches!("Satisfaction", Kind, Ok(Kind::Satisfaction));
    assert_matches!("Value", Kind, Ok(Kind::Value));
    assert_matches!("Macro", Kind, Ok(Kind::Macro));
    assert_matches!(
        "(-> * * *)",
        Kind,
        Ok(Kind::Fun(KindFun {
            args: [_, _],
            ret: _,
            ..
        }))
    );
}

#[test]
fn test_expr() {
    assert_matches!(
        "foo",
        Expr,
        Ok(Expr::Use(Use::Name(Name { sym: "foo", .. })))
    );
    assert_matches!(
        "(f a b c)",
        Expr,
        Ok(Expr::App(ExprApply {
            args: [_, _, _],
            ..
        }))
    );
    assert_matches!("123", Expr, Ok(Expr::Literal(_)));
    assert_matches!("#\\a", Expr, Ok(Expr::Literal(_)));
    assert_matches!("'x", Expr, Ok(Expr::Quote(_)));
    assert_matches!(
        r#"(begin a b c)"#,
        Expr,
        Ok(Expr::Begin(ExprBegin {
            body: [_, _, _],
            ..
        }))
    );
    assert_matches!(
        r#"(let ([a 1] [b 2]) c d)"#,
        ExprLet<LocalDef>,
        Ok(ExprLet { defs: vars, body: [_, _], .. })
            if matches!(
                vars.as_slice(),
                [
                    LocalDef { name: Name { sym: "a", .. }, params: None, .. },
                    LocalDef { name: Name { sym: "b", .. }, params: None, .. },
                ]
            )
    );
    assert_matches!(
        r#"(let ([(f x) 1] [(g a b) {T} 2]))"#,
        ExprLet<LocalDef>,
        Ok(ExprLet { defs: vars, body: [], .. })
            if matches!(
                vars.as_slice(),
                [
                    LocalDef { name: Name { sym: "f", .. }, params: Some(f_params), scheme: None, .. },
                    LocalDef { name: Name { sym: "g", .. }, params: Some(g_params), scheme: Some(_), .. },
                ]
                    if matches!(
                       f_params.as_slice(),
                       [Parameter { name: Name { sym: "x", .. }, .. }]
                    )
                    && matches!(
                        g_params.as_slice(),
                        [
                            Parameter { name: Name { sym: "a", .. }, .. },
                            Parameter { name: Name { sym: "b", .. }, .. }
                        ]
                    )
            )
    );
    assert_matches!(r#"(if a b c)"#, Expr, Ok(Expr::If(ExprIf { .. })));
    assert_matches!(
        r#"(while a b c d)"#,
        Expr,
        Ok(Expr::While(ExprWhile {
            body: [_, _, _],
            ..
        }))
    );
    assert_matches!(
        r#"(match expr [A 0] [B 1])"#,
        ExprMatch<MatchClause>,
        Ok(ExprMatch { target: _, clauses, .. })
            if matches!(clauses.as_slice(), [MatchClause { .. }, MatchClause { .. }])
    );
    assert_matches!(r#"(return)"#, ExprReturn, Ok(None));
    assert_matches!(r#"(return 123)"#, ExprReturn, Ok(Some(_)));
    assert_matches!(
        r#"(: a b)"#,
        Expr,
        Ok(Expr::Tuple(ExprTuple { exprs: [_, _], .. }))
    );
    assert_matches!(r#"unit"#, Expr, Ok(Expr::Unit));
    assert_matches!(r#"\foo"#, Expr, Ok(Expr::Capture(Name { sym: "foo", .. })));
    assert_matches!(
        r#"foo {bar}"#,
        Expr,
        Ok(Expr::Annotate(ExprAnnotate { .. }))
    );
}

#[test]
fn test_pattern() {
    assert_matches!("_", Pattern, Ok(Pattern::Wildcard));
    assert_matches!("123", Pattern, Ok(Pattern::Const(_)));
    assert_matches!("\"foo\"", Pattern, Ok(Pattern::Const(_)));
    assert_matches!(
        "(let x)",
        Pattern,
        Ok(Pattern::Var(PatternVar {
            name: Name { sym: "x", .. },
            as_pat: None,
            ..
        }))
    );
    assert_matches!(
        "(let x 123)",
        Pattern,
        Ok(Pattern::Var(PatternVar {
            name: Name { sym: "x", .. },
            as_pat: Some(_),
            ..
        }))
    );
    assert_matches!(
        "nil",
        Pattern,
        Ok(Pattern::Decon(PatternDecon {
            con: Use::Name(Name { sym: "nil", .. }),
            fields: None,
            ..
        }))
    );
    assert_matches!(
        "(cons (let x) (let xs))",
        Pattern,
        Ok(Pattern::Decon(PatternDecon {
            con: Use::Name(Name { sym: "cons", .. }),
            fields: Some([_, _]),
            ..
        }))
    );
}

#[test]
fn test_import() {
    assert_matches!(
        r#"(import "hello" foo bar (foobar baz))"#,
        Import<PortTarget>,
        Ok(Import { path: "hello", targets, .. })
            if matches!(targets.as_slice(), [
                PortTarget { name: None, target: Name { sym: "foo", ..}, .. },
                PortTarget { name: None, target: Name { sym: "bar", ..}, .. },
                PortTarget { name: Some(Name { sym: "foobar", .. }), target: Name { sym: "baz", .. }, .. },
            ])
    );
}

#[test]
fn test_export() {
    assert_matches!(
        r#"(export (hoge fuga) piyo)"#,
        Export<PortTarget>,
        Ok(Export { targets, .. })
            if matches!(targets.as_slice(), [
                PortTarget { name: Some(Name { sym: "hoge", .. }), target: Name { sym: "fuga", .. }, .. },
                PortTarget { name: None, target: Name { sym: "piyo", .. }, .. },
            ])
    );
}

#[test]
fn test_function() {
    assert_matches!(
        r#"(function f f)"#,
        Function,
        Ok(Function {
            transparent: false,
            name: Name { sym: "f", .. },
            params: None,
            scheme: None,
            body: [_],
            ..
        })
    );

    assert_matches!(
        r#"(transparent-function f f)"#,
        Function,
        Ok(Function {
            transparent: true,
            name: Name { sym: "f", .. },
            params: None,
            scheme: None,
            body: [_],
            ..
        })
    );

    assert_matches!(
        r#"(function (f a b) a)"#,
        Function,
        Ok(Function { name: Name { sym: "f", .. }, params: Some(params), scheme: None, body: [_], .. })
            if matches!(
                params.as_slice(),
                [
                    Parameter { name: Name { sym: "a", .. }, .. },
                    Parameter { name: Name { sym: "b", .. }, .. },
                ]
            )
    );

    assert_matches!(
        r#"(function (f a b) {Ty} a)"#,
        Function,
        Ok(Function { name: Name { sym: "f", .. }, params: Some(params), scheme: Some(_), body: [_], .. })
            if matches!(
                params.as_slice(),
                [
                    Parameter { name: Name { sym: "a", .. }, .. },
                    Parameter { name: Name { sym: "b", .. }, .. },
                ]
            )
    );
}

#[test]
fn test_c_function() {
    assert_matches!(
        r#"(c-function puts {(-> (Ptr U8) I32)} "puts")"#,
        CFunction,
        Ok(CFunction {
            name: Name { sym: "puts", .. },
            c_name: "puts",
            ..
        })
    );
}

#[test]
fn test_builtin_op() {
    assert_matches!(
        r#"(builtin-op add {(-> I32 I32 I32)} "i32.add")"#,
        BuiltinOp,
        Ok(BuiltinOp {
            name: Name { sym: "add", .. },
            scheme: Scheme { body, .. },
            builtin_name: "i32.add",
            ..
        })
            if matches!(body.rep, SexpRep::List(_))
    );
}

#[test]
fn test_macro() {
    assert_matches!(
        r#"(macro (f a) (cdr a))"#,
        Macro,
        Ok(Macro {
            name: Name { sym: "f", .. },
            param: Parameter {
                name: Name { sym: "a", .. },
                ..
            },
            ..
        })
    );
}

#[test]
fn test_data() {
    assert_matches!(
        r#"(value-data Bool true false)"#,
        Data<DataValueConstructor>,
        Ok(Data {
            repr: DataRepr::Value,
            name: Name { sym: "Bool", .. },
            ty_params: None,
            value_cons,
            ..
        })
            if matches!(
                value_cons.as_slice(),
                [
                    DataValueConstructor {
                        name: Name { sym: "true", .. },
                        fields: None,
                        ..
                    },
                    DataValueConstructor {
                        name: Name { sym: "false", .. },
                        fields: None,
                        ..
                    },
                ]
            )
    );
    assert_matches!(
        r#"(data (List A) (cons A (List A)) nil)"#,
        Data<DataValueConstructor>,
        Ok(Data {
            repr: DataRepr::Default,
            name: Name { sym: "List", .. },
            ty_params: Some(ref params),
            value_cons,
            ..
        })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && matches!(
                value_cons.as_slice(),
                [
                    DataValueConstructor { name: Name { sym: "cons", .. }, fields: Some([_, _]), .. },
                    DataValueConstructor { name: Name { sym: "nil", .. }, fields: None, .. },
                ]
            )
    );
    assert_matches!(
        r#"(c-data (Proxy (A *)))"#,
        Data<DataValueConstructor>,
        Ok(Data {
            repr: DataRepr::C,
            name: Name { sym: "Proxy", .. },
            ty_params: Some(ref params),
            value_cons,
            ..
        })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: Some(_), .. }])
            && matches!(value_cons.as_slice(), [])
    );
}

#[test]
fn test_builtin_type() {
    assert_matches!(
        r#"(builtin-type Foo "foo")"#,
        BuiltinType<BuiltinValueConstructor>,
        Ok(BuiltinType {
            name: Name { sym: "Foo", .. },
            ty_params: None,
            builtin_name: "foo",
            ..
        })
    );
    assert_matches!(
        r#"(builtin-type (Foo A) "foo")"#,
        BuiltinType<BuiltinValueConstructor>,
        Ok(BuiltinType {
            name: Name { sym: "Foo", .. },
            ty_params: Some(ref params),
            builtin_name: "foo",
            ..
        })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
    );
    assert_matches!(
        r#"(builtin-type (Foo A) "foo" [bar "baz"])"#,
        BuiltinType<BuiltinValueConstructor>,
        Ok(BuiltinType {
            name: Name { sym: "Foo", .. },
            ty_params: Some(ref params),
            builtin_name: "foo",
            value_cons,
            ..
        })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && matches!(value_cons.as_slice(), [
                BuiltinValueConstructor { name: Name { sym: "bar", .. }, fields: None, builtin_name: "baz", .. }
            ])
    );
    assert_matches!(
        r#"(builtin-type (Foo A) "foo" [(foo I32) "foo"])"#,
        BuiltinType<BuiltinValueConstructor>,
        Ok(BuiltinType {
            name: Name { sym: "Foo", .. },
            ty_params: Some(ref params),
            builtin_name: "foo",
            value_cons,
            ..
        })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && matches!(value_cons.as_slice(), [
                BuiltinValueConstructor {
                    name: Name { sym: "foo", .. },
                    fields: Some([_]),
                    builtin_name: "foo",
                    ..
                }
            ])
    );
}

#[test]
fn test_class() {
    assert_matches!(
        "(class Partial)",
        Class<Function>,
        Ok(Class { name: Name { sym: "Partial", .. }, ty_params: None, superclasses, methods, is_sealed: false, .. })
            if superclasses.is_empty()
            && methods.is_empty()
    );

    assert_matches!(
        "(sealed-class Partial)",
        Class<Function>,
        Ok(Class { name: Name { sym: "Partial", .. }, ty_params: None, superclasses, methods, is_sealed: true, .. })
            if superclasses.is_empty()
            && methods.is_empty()
    );

    assert_matches!(
        "(class (Eq A) (function (eq x y) {(-> A A Bool)}))",
        Class<Function>,
        Ok(Class { name: Name { sym: "Eq", .. }, ty_params: Some(params), superclasses, methods, .. })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && superclasses.is_empty()
            && matches!(methods.as_slice(), [_])
    );

    assert_matches!(
        "(class (Ord (A *)) (where (Eq A)) (function (compare x y) {(-> A A Ordering)}))",
        Class<Function>,
        Ok(Class { name: Name { sym: "Ord", .. }, ty_params: Some(params), superclasses, methods, .. })
            if matches!(params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: Some(_), .. }])
            && matches!(superclasses.as_slice(), [Constraint { target: Use::Name(Name { sym: "Eq", .. }), args: Some([_]), .. }])
            && matches!(methods.as_slice(), [_])
    );
}

#[test]
fn test_instance() {
    assert_matches!(
        "(instance Eq:Int (Eq Int) (function (eq x y) True))",
        Instance<Function>,
        Ok(Instance { ty_params, target, s_params, method_impls, .. })
            if ty_params.is_empty()
            && matches!(target, Constraint { target: Use::Name(Name { sym: "Eq", .. }), args: Some([_]), .. })
            && s_params.is_empty()
            && matches!(method_impls.as_slice(), [_])
    );

    assert_matches!(
        "(instance Eq:Pair (forall A) (Eq (Pair A A)) (where (Eq A) (Eq A)) (function (eq x y) true))",
        Instance<Function>,
        Ok(Instance { ty_params, target, s_params, method_impls, .. })
            if matches!(ty_params.as_slice(), [
                TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }
            ])
            && matches!(target, Constraint { target: Use::Name(Name { sym: "Eq", .. }), args: Some([_]), .. })
            && matches!(
                s_params.as_slice(),
                [
                    Constraint { target: Use::Name(Name { sym: "Eq", .. }), .. },
                    Constraint { target: Use::Name(Name { sym: "Eq", .. }), .. },
                ]
            )
            && matches!(method_impls.as_slice(), [_])
    );
}

#[test]
fn test_scheme() {
    assert_matches!(
        "(I32)",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if ty_params.is_empty()
            && s_params.is_empty()
            && matches!(body.rep, SexpRep::Symbol(_))
    );
    assert_matches!(
        "((-> I32 I32))",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if ty_params.is_empty()
            && s_params.is_empty()
            && matches!(body.rep, SexpRep::List(_))
    );
    assert_matches!(
        "((forall A) A)",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if matches!(ty_params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && s_params.is_empty()
            && matches!(body.rep, SexpRep::Symbol(_))
    );
    assert_matches!(
        "((forall (A *)) (-> A A))",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if matches!(ty_params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: Some(_), .. }])
            && s_params.is_empty()
            && matches!(body.rep, SexpRep::List(_))
    );
    assert_matches!(
        "((forall A) (-> A A Bool) (where (Eq A)))",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if matches!(ty_params.as_slice(), [TypeParameter { name: Name { sym: "A", .. }, kind_ann: None, .. }])
            && matches!(s_params.as_slice(), [Constraint { target: Use::Name(Name { sym: "Eq", .. }), args: Some([_]), .. }])
            && matches!(body.rep, SexpRep::List(_))
    );
    assert_matches!(
        "(Int (where Partial))",
        m::List<Scheme>,
        Ok(Scheme { ty_params, s_params, body, .. })
            if ty_params.is_empty()
            && matches!(s_params.as_slice(), [Constraint { target: Use::Name(Name { sym: "Partial", .. }), args: None, .. }])
            && matches!(body.rep, SexpRep::Symbol(_))
    );
}
