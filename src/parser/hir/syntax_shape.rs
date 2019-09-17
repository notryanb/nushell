use crate::parser::{hir, hir::TokensIterator, RawToken, TokenNode};
use crate::prelude::*;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum SyntaxShape {
    Any,
    CommandHead,
    List,
    Literal,
    String,
    Member,
    Variable,
    Number,
    Path,
    Pattern,
    Binary,
    Block,
    Boolean,
}

impl ExpandSyntax for SyntaxShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        match self {
            SyntaxShape::Any => AnyExpressionShape.expand(token_nodes, context, source, origin),
            SyntaxShape::CommandHead => {
                CommandHeadShape.expand(token_nodes, context, source, origin)
            }
            SyntaxShape::List => unimplemented!(),
            SyntaxShape::Literal => unimplemented!(),
            SyntaxShape::String => StringShape.expand(token_nodes, context, source, origin),
            SyntaxShape::Member => unimplemented!(),
            SyntaxShape::Variable => unimplemented!(),
            SyntaxShape::Number => NumberShape.expand(token_nodes, context, source, origin),
            SyntaxShape::Path => unimplemented!(),
            SyntaxShape::Pattern => PatternShape.expand(token_nodes, context, source, origin),
            SyntaxShape::Binary => unimplemented!(),
            SyntaxShape::Block => unimplemented!(),
            SyntaxShape::Boolean => unimplemented!(),
        }
    }
}

impl std::fmt::Display for SyntaxShape {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SyntaxShape::Any => write!(f, "Any"),
            SyntaxShape::CommandHead => write!(f, "CommandHead"),
            SyntaxShape::List => write!(f, "List"),
            SyntaxShape::Literal => write!(f, "Literal"),
            SyntaxShape::String => write!(f, "String"),
            SyntaxShape::Member => write!(f, "Member"),
            SyntaxShape::Variable => write!(f, "Variable"),
            SyntaxShape::Number => write!(f, "Number"),
            SyntaxShape::Path => write!(f, "Path"),
            SyntaxShape::Pattern => write!(f, "Pattern"),
            SyntaxShape::Binary => write!(f, "Binary"),
            SyntaxShape::Block => write!(f, "Block"),
            SyntaxShape::Boolean => write!(f, "Boolean"),
        }
    }
}

pub trait ExpandSyntax: std::fmt::Debug + Copy {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError>;
}

#[derive(Debug, Copy, Clone)]
pub struct StringShape;

impl ExpandSyntax for StringShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        _context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "String", source, origin, |token, token_tag| {
            Ok(match token {
                RawToken::GlobPattern => {
                    return Err(ShellError::type_error(
                        "String",
                        "glob pattern".tagged(token_tag),
                    ))
                }
                RawToken::Variable(tag) if tag.slice(source) == "it" => {
                    hir::Expression::it_variable(tag, token_tag)
                }
                RawToken::ExternalCommand(tag) => hir::Expression::external_command(tag, token_tag),
                RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                RawToken::Number(_) => hir::Expression::bare(token_tag),
                RawToken::Size(_, _) => hir::Expression::bare(token_tag),
                RawToken::Bare => hir::Expression::bare(token_tag),
                RawToken::String(tag) => hir::Expression::string(tag, token_tag),
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NumberShape;

impl ExpandSyntax for NumberShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        _context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "Number", source, origin, |token, token_tag| {
            Ok(match token {
                RawToken::GlobPattern => {
                    return Err(ShellError::type_error(
                        "Number",
                        "glob pattern".to_string().tagged(token_tag),
                    ))
                }
                RawToken::Variable(tag) if tag.slice(source) == "it" => {
                    hir::Expression::it_variable(tag, token_tag)
                }
                RawToken::ExternalCommand(tag) => hir::Expression::external_command(tag, token_tag),
                RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                RawToken::Number(number) => {
                    hir::Expression::number(number.to_number(source), token_tag)
                }
                RawToken::Size(number, unit) => {
                    hir::Expression::size(number.to_number(source), unit, token_tag)
                }
                RawToken::Bare => hir::Expression::bare(token_tag),
                RawToken::String(tag) => hir::Expression::string(tag, token_tag),
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PatternShape;

impl ExpandSyntax for PatternShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(
            token_nodes,
            "Pattern",
            source,
            origin,
            |token, token_tag| {
                Ok(match token {
                    RawToken::Variable(tag) if tag.slice(source) == "it" => {
                        hir::Expression::it_variable(tag, token_tag)
                    }
                    RawToken::ExternalCommand(_) => {
                        return Err(ShellError::syntax_error(
                            "Invalid external command".to_string().tagged(token_tag),
                        ))
                    }
                    RawToken::ExternalWord => {
                        return Err(ShellError::invalid_external_word(token_tag))
                    }
                    RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                    RawToken::Number(_) => hir::Expression::bare(token_tag),
                    RawToken::Size(_, _) => hir::Expression::bare(token_tag),
                    RawToken::GlobPattern => hir::Expression::pattern(token_tag),
                    RawToken::Bare => hir::Expression::file_path(
                        expand_path(token_tag.slice(source), context),
                        token_tag,
                    ),
                    RawToken::String(tag) => hir::Expression::file_path(
                        expand_path(tag.slice(source), context),
                        token_tag,
                    ),
                })
            },
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PathShape;

impl ExpandSyntax for PathShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "Path", source, origin, |token, token_tag| {
            Ok(match token {
                RawToken::Variable(tag) if tag.slice(source) == "it" => {
                    hir::Expression::it_variable(tag, token_tag)
                }
                RawToken::ExternalCommand(tag) => hir::Expression::external_command(tag, token_tag),
                RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                RawToken::Number(_) => hir::Expression::bare(token_tag),
                RawToken::Size(_, _) => hir::Expression::bare(token_tag),
                RawToken::Bare => hir::Expression::file_path(
                    expand_path(token_tag.slice(source), context),
                    token_tag,
                ),
                RawToken::GlobPattern => {
                    return Err(ShellError::type_error(
                        "Path",
                        "glob pattern".tagged(token_tag),
                    ))
                }
                RawToken::String(tag) => {
                    hir::Expression::file_path(expand_path(tag.slice(source), context), token_tag)
                }
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CommandHeadShape;

impl ExpandSyntax for CommandHeadShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(
            token_nodes,
            "command head",
            source,
            origin,
            |token, token_tag| {
                Ok(match token {
                    RawToken::ExternalCommand(tag) => {
                        hir::Expression::external_command(tag, token_tag)
                    }
                    RawToken::Bare => hir::RawExpression::Command.tagged(token_tag),
                    _ => {
                        return Err(ShellError::type_error(
                            "command head",
                            token.type_name().tagged(token_tag),
                        ))
                    }
                })
            },
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct InternalCommandHeadShape;

impl ExpandSyntax for InternalCommandHeadShape {
    fn expand(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        let peeked_head = token_nodes
            .peek_non_ws()
            .ok_or_else(|| ShellError::unexpected_eof("command head", origin))?;

        let expr = match peeked_head.node {
            TokenNode::Token(
                spanned @ Tagged {
                    item: RawToken::Bare,
                    ..
                },
            ) => spanned.map(|_| hir::RawExpression::Literal(hir::Literal::Bare)),

            TokenNode::Token(Tagged {
                item: RawToken::String(inner_tag),
                tag,
            }) => hir::RawExpression::Literal(hir::Literal::String(*inner_tag)).tagged(*tag),

            node => {
                return Err(ShellError::type_error(
                    "command head",
                    node.tagged_type_name(),
                ))
            }
        };

        peeked_head.commit();

        Ok(expr)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct AnyExpressionShape;

impl ExpandSyntax for AnyExpressionShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &Context,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        unimplemented!()
    }
}

pub fn expand_path(string: &str, context: &Context) -> PathBuf {
    let expanded = shellexpand::tilde_with_context(string, || context.shell_manager.homedir());

    PathBuf::from(expanded.as_ref())
}

fn parse_single_node<'a, 'b>(
    token_nodes: &'b mut TokensIterator<'a>,
    expected: &'static str,
    source: &Text,
    origin: uuid::Uuid,
    callback: impl FnOnce(RawToken, Tag) -> Result<hir::Expression, ShellError>,
) -> Result<hir::Expression, ShellError> {
    let peeked = token_nodes
        .peek_any()
        .ok_or_else(|| ShellError::unexpected_eof("String", origin))?;

    let expr = match peeked.node {
        TokenNode::Token(token) => callback(token.item, token.tag())?,

        other => return Err(ShellError::type_error(expected, other.tagged_type_name())),
    };

    peeked.commit();

    Ok(expr)
}
