use crate::context::Context;
use crate::errors::ShellError;
use crate::parser::{
    hir,
    hir::{
        baseline_parse_single_token, baseline_parse_token_as_command_head,
        baseline_parse_token_as_number, baseline_parse_token_as_path,
        baseline_parse_token_as_pattern, baseline_parse_token_as_string,
    },
    DelimitedNode, Delimiter, PathNode, RawToken, SyntaxShape, TokenNode, TokensIterator,
};
use crate::{Tag, Tagged, TaggedItem, Text};
use derive_new::new;
use log::trace;
use serde::{Deserialize, Serialize};

pub fn expand_external_tokens(
    token_nodes: &mut TokensIterator<'_>,
    source: &Text,
) -> Result<Vec<Tagged<String>>, ShellError> {
    let mut out: Vec<Tagged<String>> = vec![];

    loop {
        if let Some(tag) = expand_next_expression(token_nodes)? {
            out.push(tag.tagged_string(source));
        } else {
            break;
        }
    }

    Ok(out)
}

pub fn expand_next_expression(
    token_nodes: &mut TokensIterator<'_>,
) -> Result<Option<Tag>, ShellError> {
    let first = token_nodes.next_non_ws();

    let first = match first {
        None => return Ok(None),
        Some(v) => v,
    };

    let first = triage_external_head(first)?;
    let mut last = first;

    loop {
        let continuation = triage_continuation(token_nodes)?;

        if let Some(continuation) = continuation {
            last = continuation;
        } else {
            break;
        }
    }

    Ok(Some(first.until(last)))
}

fn triage_external_head(node: &TokenNode) -> Result<Tag, ShellError> {
    Ok(match node {
        TokenNode::Token(token) => token.tag(),
        TokenNode::Call(call) => unimplemented!(),
        TokenNode::Nodes(nodes) => unimplemented!(),
        TokenNode::Delimited(delimited) => unimplemented!(),
        TokenNode::Pipeline(pipeline) => unimplemented!(),
        TokenNode::Flag(flag) => flag.tag(),
        TokenNode::Member(member) => *member,
        TokenNode::Whitespace(whitespace) => {
            unreachable!("This function should be called after next_non_ws()")
        }
        TokenNode::Error(error) => unimplemented!(),
    })
}

fn triage_continuation<'a, 'b>(
    nodes: &'a mut TokensIterator<'b>,
) -> Result<Option<Tag>, ShellError> {
    let peeked = nodes.peek_any();

    let peeked = match peeked {
        None => return Ok(None),
        Some(peeked) => peeked,
    };

    match &peeked {
        peeked if peeked.node.is_whitespace() => return Ok(None),
        peeked => match &peeked.node {
            TokenNode::Token(..) | TokenNode::Flag(..) | TokenNode::Member(..) => {}
            TokenNode::Call(..) => unimplemented!("call"),
            TokenNode::Nodes(..) => unimplemented!("nodes"),
            TokenNode::Delimited(..) => unimplemented!("delimited"),
            TokenNode::Pipeline(..) => unimplemented!("pipeline"),
            TokenNode::Whitespace(..) => unimplemented!("whitespace"),
            TokenNode::Error(..) => unimplemented!("error"),
        },
    }

    let next = peeked.commit();
    Ok(Some(next.tag()))
}
