use crate::commands::Command;
use crate::context::Context;
use crate::errors::{ArgumentError, ShellError};
use crate::parser::registry::{NamedType, PositionalType, Signature};
use crate::parser::{baseline_parse_tokens, TokensIterator};
use crate::parser::{
    hir::{self, ExpandContext, Literal, NamedArguments, RawExpression},
    Flag, RawToken, TokenNode,
};
use crate::traits::ToDebug;
use crate::{Tag, Tagged, TaggedItem, Text};
use log::trace;
use std::sync::Arc;

pub fn parse_command(
    context: &Context,
    head: Tagged<RawExpression>,
    command: Arc<Command>,
    body: Tagged<&mut TokensIterator>,
    source: &Text,
) -> Result<(hir::Call, Arc<Command>), ShellError> {
    trace!("Processing {:?}", command.signature());

    let call = match parse_command_tail(
        &command.signature(),
        &context.expand_context(body.tag.origin),
        body.item,
        source,
        body.tag(),
    )? {
        None => hir::Call::new(Box::new(head), None, None),
        Some((positional, named)) => hir::Call::new(Box::new(head), positional, named),
    };

    Ok((call, command))
}

pub fn parse_command_tail(
    config: &Signature,
    context: &ExpandContext,
    tail: &mut TokensIterator,
    source: &Text,
    command_tag: Tag,
) -> Result<Option<(Option<Vec<hir::Expression>>, Option<NamedArguments>)>, ShellError> {
    let mut named = NamedArguments::new();
    let origin = command_tag.origin;

    trace_remaining("nodes", tail.clone(), source);

    for (name, kind) in &config.named {
        trace!(target: "nu::parse", "looking for {} : {:?}", name, kind);

        match kind {
            NamedType::Switch => {
                let flag = extract_switch(name, tail, source);

                named.insert_switch(name, flag);
            }
            NamedType::Mandatory(syntax_type) => {
                match extract_mandatory(config, name, tail, source, command_tag) {
                    Err(err) => return Err(err), // produce a correct diagnostic
                    Ok((pos, flag)) => {
                        tail.move_to(pos);

                        if tail.at_end() {
                            return Err(ShellError::argument_error(
                                config.name.clone(),
                                ArgumentError::MissingValueForName(name.to_string()),
                                flag.tag(),
                            ));
                        }

                        let expr = hir::baseline_parse_next_expr(
                            tail,
                            context,
                            source,
                            origin,
                            *syntax_type,
                        )?;

                        tail.restart();
                        named.insert_mandatory(name, expr);
                    }
                }
            }
            NamedType::Optional(syntax_type) => match extract_optional(name, tail, source) {
                Err(err) => return Err(err), // produce a correct diagnostic
                Ok(Some((pos, flag))) => {
                    tail.move_to(pos);

                    if tail.at_end() {
                        return Err(ShellError::argument_error(
                            config.name.clone(),
                            ArgumentError::MissingValueForName(name.to_string()),
                            flag.tag(),
                        ));
                    }

                    let expr =
                        hir::baseline_parse_next_expr(tail, context, source, origin, *syntax_type)?;

                    tail.restart();
                    named.insert_optional(name, Some(expr));
                }

                Ok(None) => {
                    tail.restart();
                    named.insert_optional(name, None);
                }
            },
        };
    }

    trace_remaining("after named", tail.clone(), source);

    let mut positional = vec![];

    for arg in &config.positional {
        trace!("Processing positional {:?}", arg);

        match arg {
            PositionalType::Mandatory(..) => {
                if tail.at_end() {
                    return Err(ShellError::argument_error(
                        config.name.clone(),
                        ArgumentError::MissingMandatoryPositional(arg.name().to_string()),
                        command_tag,
                    ));
                }
            }

            PositionalType::Optional(..) => {
                if tail.at_end() {
                    break;
                }
            }
        }

        let result =
            hir::baseline_parse_next_expr(tail, context, source, origin, arg.syntax_type())?;

        positional.push(result);
    }

    trace_remaining("after positional", tail.clone(), source);

    if let Some(syntax_type) = config.rest_positional {
        let remainder = baseline_parse_tokens(tail, context, source, origin, syntax_type)?;
        positional.extend(remainder);
    }

    trace_remaining("after rest", tail.clone(), source);

    trace!("Constructed positional={:?} named={:?}", positional, named);

    let positional = if positional.len() == 0 {
        None
    } else {
        Some(positional)
    };

    // TODO: Error if extra unconsumed positional arguments

    let named = if named.named.is_empty() {
        None
    } else {
        Some(named)
    };

    trace!("Normalized positional={:?} named={:?}", positional, named);

    Ok(Some((positional, named)))
}

fn extract_switch(name: &str, tokens: &mut hir::TokensIterator<'_>, source: &Text) -> Option<Flag> {
    tokens
        .extract(|t| t.as_flag(name, source))
        .map(|(_pos, flag)| flag.item)
}

fn extract_mandatory(
    config: &Signature,
    name: &str,
    tokens: &mut hir::TokensIterator<'_>,
    source: &Text,
    tag: Tag,
) -> Result<(usize, Tagged<Flag>), ShellError> {
    let flag = tokens.extract(|t| t.as_flag(name, source));

    match flag {
        None => Err(ShellError::argument_error(
            config.name.clone(),
            ArgumentError::MissingMandatoryFlag(name.to_string()),
            tag,
        )),

        Some((pos, flag)) => {
            tokens.remove(pos);
            Ok((pos, flag))
        }
    }
}

fn extract_optional(
    name: &str,
    tokens: &mut hir::TokensIterator<'_>,
    source: &Text,
) -> Result<(Option<(usize, Tagged<Flag>)>), ShellError> {
    let flag = tokens.extract(|t| t.as_flag(name, source));

    match flag {
        None => Ok(None),
        Some((pos, flag)) => {
            tokens.remove(pos);
            Ok(Some((pos, flag)))
        }
    }
}

pub fn trace_remaining(desc: &'static str, tail: hir::TokensIterator<'_>, source: &Text) {
    trace!(
        "{} = {:?}",
        desc,
        itertools::join(
            tail.debug_remaining()
                .iter()
                .map(|i| format!("%{}%", i.debug(&source))),
            " "
        )
    );
}
