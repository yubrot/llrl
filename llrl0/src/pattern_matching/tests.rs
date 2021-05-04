use super::*;
use crate::ast;
use std::rc::Rc;
use Pattern::*;

fn check(pats: Vec<Pattern>) -> Result<(), Error> {
    super::check(
        pats.into_iter()
            .map(|pat| Row::new(vec![pat].into()))
            .collect(),
    )
}

fn non_exhaustive(pats: Vec<Pattern>) -> Error {
    Error::NonExhaustive(Matrix::new(
        pats.into_iter().map(|p| Row::new(vec![p].into())).collect(),
    ))
}

fn useless(pat: Pattern) -> Error {
    Error::Useless(Row::new(vec![pat].into()))
}

fn maybe(id_gen: &mut ast::NodeIdGenerator) -> (Tag, Tag) {
    let just = FiniteCon::new(ast::ValueCon::Data(id_gen.generate()), 1);
    let nothing = FiniteCon::new(ast::ValueCon::Data(id_gen.generate()), 0);
    let cons = vec![just, nothing].into();

    let just = Tag::Finite(just, Rc::clone(&cons));
    let nothing = Tag::Finite(nothing, Rc::clone(&cons));
    (just, nothing)
}

fn pair(id_gen: &mut ast::NodeIdGenerator) -> Tag {
    let pair = FiniteCon::new(ast::ValueCon::Data(id_gen.generate()), 2);
    let cons = vec![pair].into();

    let pair = Tag::Finite(pair, Rc::clone(&cons));
    pair
}

#[test]
fn test_exhaustiveness() {
    let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
    let (just_tag, nothing_tag) = maybe(&mut id_gen);
    let pair_tag = pair(&mut id_gen);

    let just = |p| Constructor(just_tag.clone(), vec![p]);
    let nothing = || Constructor(nothing_tag.clone(), Vec::new());
    let pair = |a, b| Constructor(pair_tag.clone(), vec![a, b]);

    assert_eq!(check(vec![Wildcard]), Ok(()));
    assert_eq!(check(vec![just(Wildcard), nothing()]), Ok(()));
    assert_eq!(
        check(vec![just(Wildcard)]),
        Err(non_exhaustive(vec![nothing()]))
    );
    assert_eq!(
        check(vec![nothing()]),
        Err(non_exhaustive(vec![just(Wildcard)]))
    );
    assert_eq!(
        check(vec![just(just(Wildcard)), nothing()]),
        Err(non_exhaustive(vec![just(nothing())]))
    );
    assert_eq!(
        check(vec![just(just(just(Wildcard))), nothing()]),
        Err(non_exhaustive(vec![just(nothing())])) // TODO: cover just(just(nothing()))
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), nothing()),
            pair(nothing(), Wildcard)
        ]),
        Err(non_exhaustive(vec![pair(just(Wildcard), just(Wildcard))]))
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), nothing()),
            pair(nothing(), Wildcard),
            pair(just(Wildcard), just(Wildcard))
        ]),
        Ok(())
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), nothing()),
            pair(nothing(), just(Wildcard))
        ]),
        Err(non_exhaustive(vec![
            pair(just(Wildcard), just(Wildcard)),
            pair(nothing(), nothing())
        ]))
    );
    assert_eq!(
        check(vec![Constructor(
            Tag::Const(ast::Const::String("foo".to_string())),
            Vec::new()
        )]),
        Err(non_exhaustive(vec![Wildcard]))
    );
    assert_eq!(
        check(vec![
            Constructor(
                Tag::Const(ast::Const::String("foo".to_string())),
                Vec::new()
            ),
            Wildcard
        ]),
        Ok(())
    );
}

#[test]
fn test_useless() {
    let mut id_gen = ast::NodeIdGenerator::in_module(ast::ModuleId::from_index(1));
    let (just_tag, nothing_tag) = maybe(&mut id_gen);
    let pair_tag = pair(&mut id_gen);

    let just = |p| Constructor(just_tag.clone(), vec![p]);
    let nothing = || Constructor(nothing_tag.clone(), Vec::new());
    let pair = |a, b| Constructor(pair_tag.clone(), vec![a, b]);

    assert_eq!(check(vec![Wildcard, Wildcard]), Err(useless(Wildcard)));
    assert_eq!(
        check(vec![just(Wildcard), just(Wildcard), Wildcard]),
        Err(useless(just(Wildcard)))
    );
    assert_eq!(
        check(vec![just(just(Wildcard)), just(Wildcard), nothing()]),
        Ok(())
    );
    assert_eq!(
        check(vec![just(Wildcard), just(just(Wildcard)), nothing()]),
        Err(useless(just(just(Wildcard))))
    );
    assert_eq!(
        check(vec![
            Constructor(Tag::Const(ast::Const::Integer(true, 0)), Vec::new()),
            Constructor(Tag::Const(ast::Const::Integer(true, 0)), Vec::new()),
            Wildcard
        ]),
        Err(useless(Constructor(
            Tag::Const(ast::Const::Integer(true, 0)),
            Vec::new()
        )))
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), nothing()),
            pair(just(just(Wildcard)), just(Wildcard)),
            Wildcard
        ]),
        Ok(())
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), Wildcard),
            pair(just(just(Wildcard)), just(Wildcard)),
            Wildcard
        ]),
        Err(useless(pair(just(just(Wildcard)), just(Wildcard))))
    );
    assert_eq!(
        check(vec![
            pair(just(Wildcard), nothing()),
            pair(nothing(), just(Wildcard)),
            pair(nothing(), nothing()),
            pair(just(Wildcard), just(Wildcard)),
        ]),
        Ok(())
    );
}
