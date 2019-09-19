use crate::parser::hir::tokens_iterator::Peeked;
use crate::parser::{hir, hir::TokensIterator, Operator, RawToken, TokenNode};
use crate::prelude::*;
use derive_new::new;
use getset::Getters;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

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

impl ExpandExpression for SyntaxShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
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

#[derive(Getters, new)]
pub struct ExpandContext<'context> {
    #[get = "pub(crate)"]
    registry: &'context CommandRegistry,
    #[get = "pub(crate)"]
    origin: uuid::Uuid,
    homedir: Option<PathBuf>,
}

impl<'context> ExpandContext<'context> {
    pub(crate) fn homedir(&self) -> Option<&Path> {
        self.homedir.as_ref().map(|h| h.as_path())
    }

    #[cfg(test)]
    pub fn with_empty(callback: impl FnOnce(ExpandContext)) {
        let registry = CommandRegistry::new();

        callback(ExpandContext {
            registry: &registry,
            homedir: None,
        })
    }
}

pub trait TestSyntax: std::fmt::Debug + Copy {
    fn test<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Option<Peeked<'a, 'b>>;
}

pub trait ExpandExpression: std::fmt::Debug + Copy {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError>;
}

pub trait ExpandSyntax: std::fmt::Debug + Copy {
    type Output;

    fn expand<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<Self::Output, ShellError>;
}

pub trait SkipSyntax: std::fmt::Debug + Copy {
    fn skip<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<(), ShellError>;
}

#[derive(Debug, Copy, Clone)]
pub struct BareShape;

impl TestSyntax for BareShape {
    fn test<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Option<Peeked<'a, 'b>> {
        match token_nodes.peek_any() {
            Some(peeked) => match peeked.node {
                TokenNode::Token(token) => match token.item {
                    RawToken::Bare => Some(peeked),
                    _ => None,
                },
                _ => None,
            },

            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DotShape;

impl SkipSyntax for DotShape {
    fn skip<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<(), ShellError> {
        parse_single_node(token_nodes, "dot", source, origin, |token, token_tag| {
            Ok(match token {
                _ => {
                    return Err(ShellError::type_error(
                        "variable",
                        token.type_name().tagged(token_tag),
                    ))
                }
            })
        })?;

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct StringShape;

impl ExpandExpression for StringShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        _context: &ExpandContext,
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
                RawToken::Operator(..) => {
                    return Err(ShellError::type_error(
                        "String",
                        "operator".tagged(token_tag),
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

impl TestSyntax for StringShape {
    fn test<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Option<Peeked<'a, 'b>> {
        match token_nodes.peek_any() {
            Some(peeked) => match peeked.node {
                TokenNode::Token(token) => match token.item {
                    RawToken::String(_) => Some(peeked),
                    _ => None,
                },
                _ => None,
            },

            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NumberShape;

impl ExpandExpression for NumberShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        _context: &ExpandContext,
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
                RawToken::Operator(..) => {
                    return Err(ShellError::type_error(
                        "Number",
                        "operator".to_string().tagged(token_tag),
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

impl ExpandExpression for PatternShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
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
                    RawToken::Operator(..) => {
                        return Err(ShellError::type_error(
                            "glob pattern",
                            "operator".to_string().tagged(token_tag),
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
                        expand_file_path(token_tag.slice(source), context),
                        token_tag,
                    ),
                    RawToken::String(tag) => hir::Expression::file_path(
                        expand_file_path(tag.slice(source), &context),
                        token_tag,
                    ),
                })
            },
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FilePathShape;

impl ExpandExpression for FilePathShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(token_nodes, "Path", source, origin, |token, token_tag| {
            Ok(match token {
                RawToken::GlobPattern => {
                    return Err(ShellError::type_error(
                        "Path",
                        "glob pattern".tagged(token_tag),
                    ))
                }
                RawToken::Operator(..) => {
                    return Err(ShellError::type_error("Path", "operator".tagged(token_tag)))
                }
                RawToken::Variable(tag) if tag.slice(source) == "it" => {
                    hir::Expression::it_variable(tag, token_tag)
                }
                RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                RawToken::ExternalCommand(tag) => hir::Expression::external_command(tag, token_tag),
                RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
                RawToken::Number(_) => hir::Expression::bare(token_tag),
                RawToken::Size(_, _) => hir::Expression::bare(token_tag),
                RawToken::Bare => hir::Expression::file_path(
                    expand_file_path(token_tag.slice(source), context),
                    token_tag,
                ),

                RawToken::String(tag) => hir::Expression::file_path(
                    expand_file_path(tag.slice(source), context),
                    token_tag,
                ),
            })
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VariablePathShape;

impl ExpandExpression for VariablePathShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        // 1. let the head be the first token, expecting a variable
        // 2. let the tail be an empty list of members
        // 2. while the next token (excluding ws) is a dot:
        //   1. consume the dot
        //   2. consume the next token as a member and push it onto tail

        let head = VariableShape.expand(token_nodes, context, source, origin)?;
        let start = head.tag();
        let mut end = start;
        let mut tail: Vec<Tagged<String>> = vec![];

        loop {
            println!("end={:?} tail={:?}", end, tail);

            match DotShape.skip(token_nodes, context, source, origin) {
                Err(_) => break,
                Ok(_) => {}
            }

            let member = MemberShape.expand(token_nodes, context, source, origin)?;
            end = member.tag();
            tail.push(member);
        }

        Ok(hir::Expression::path(head, tail, start.until(end)))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VariableShape;

impl ExpandExpression for VariableShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        parse_single_node(
            token_nodes,
            "variable",
            source,
            origin,
            |token, token_tag| {
                Ok(match token {
                    RawToken::Variable(tag) => hir::Expression::variable(tag, token_tag),
                    _ => {
                        return Err(ShellError::type_error(
                            "variable",
                            token.type_name().tagged(token_tag),
                        ))
                    }
                })
            },
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemberShape;

impl ExpandSyntax for MemberShape {
    type Output = Tagged<String>;

    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<Tagged<String>, ShellError> {
        let bare = BareShape.test(token_nodes, context, source, origin);
        if let Some(bare) = bare {
            let bare = bare.commit();

            return Ok(bare.tag().slice(source).to_string().tagged(bare.tag()));
        }

        let string = StringShape.test(token_nodes, context, source, origin);

        if let Some(string) = string {
            let string = string.commit();
            let (outer, inner) = string.expect_string();

            return Ok(inner.slice(source).to_string().tagged(outer));
        }

        Err(ShellError::syntax_error())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CommandHeadShape;

impl ExpandExpression for CommandHeadShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
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

impl ExpandExpression for InternalCommandHeadShape {
    fn expand(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
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

impl ExpandExpression for AnyExpressionShape {
    fn expand<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
        source: &Text,
        origin: uuid::Uuid,
    ) -> Result<hir::Expression, ShellError> {
        unimplemented!()
    }
}

pub fn expand_file_path(string: &str, context: &ExpandContext) -> PathBuf {
    let expanded = shellexpand::tilde_with_context(string, || context.homedir());

    PathBuf::from(expanded.as_ref())
}

fn parse_single_node<'a, 'b, T>(
    token_nodes: &'b mut TokensIterator<'a>,
    expected: &'static str,
    source: &Text,
    origin: uuid::Uuid,
    callback: impl FnOnce(RawToken, Tag) -> Result<T, ShellError>,
) -> Result<T, ShellError> {
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
