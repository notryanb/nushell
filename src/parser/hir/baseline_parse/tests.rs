use crate::parser::parse::token_tree_builder::TokenTreeBuilder as b;

#[test]
fn test_parse_path() {
    let tokens = b::token_list(vec![b::var("it"), b::op("."), b::bare("cpu")]);
}
