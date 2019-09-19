use crate::parser::hir;
use crate::parser::hir::syntax_shape::*;
use crate::parser::hir::TokensIterator;
use crate::parser::parse::token_tree_builder::{CurriedToken, TokenTreeBuilder as b};
use crate::parser::TokenNode;
use crate::{Span, Tag, Tagged, TaggedItem, Text};
use pretty_assertions::assert_eq;
use uuid::Uuid;

#[test]
fn test_parse_string() {
    parse_tokens(StringShape, vec![b::string("hello")], |tokens| {
        hir::Expression::string(inner_string_tag(tokens[0].tag()), tokens[0].tag())
    });
}

#[test]
fn test_parse_path() {
    parse_tokens(
        VariablePathShape,
        vec![b::var("it"), b::op("."), b::bare("cpu")],
        |tokens| {
            let (outer_var, inner_var) = tokens[0].expect_var();
            let bare = tokens[2].expect_bare();
            hir::Expression::path(
                hir::Expression::variable(inner_var, outer_var),
                vec!["cpu".tagged(bare)],
                outer_var.until(bare),
            )
        },
    );

    parse_tokens(
        VariablePathShape,
        vec![
            b::var("cpu"),
            b::op("."),
            b::bare("amount"),
            b::op("."),
            b::string("max ghz"),
        ],
        |tokens| {
            let (outer_var, inner_var) = tokens[0].expect_var();
            let amount = tokens[2].expect_bare();
            let (outer_max_ghz, _) = tokens[4].expect_string();

            hir::Expression::path(
                hir::Expression::variable(inner_var, outer_var),
                vec!["amount".tagged(amount), "max ghz".tagged(outer_max_ghz)],
                outer_var.until(outer_max_ghz),
            )
        },
    );
}

fn parse_tokens(
    shape: impl ExpandExpression,
    tokens: Vec<CurriedToken>,
    expected: impl FnOnce(Tagged<&[TokenNode]>) -> hir::Expression,
) {
    ExpandContext::with_empty(|context| {
        let tokens = b::token_list(tokens);
        let (tokens, source) = b::build(test_origin(), tokens);
        let tokens = tokens.expect_list();
        let mut iterator = TokensIterator::all(tokens.item, *context.origin());

        let expr = shape
            .expand(&mut iterator, &context, &Text::from(source), test_origin())
            .unwrap();

        assert_eq!(expr, expected(tokens));
    })
}

fn test_origin() -> Uuid {
    Uuid::nil()
}

fn inner_string_tag(tag: Tag) -> Tag {
    Tag {
        span: Span {
            start: tag.span.start + 1,
            end: tag.span.end - 1,
        },
        origin: tag.origin,
    }
}
