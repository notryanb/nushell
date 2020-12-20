use crate::commands::WholeStreamCommand;
use crate::prelude::*;
use nu_errors::ShellError;
use nu_protocol::ShellTypeName;
use nu_protocol::{
    ColumnPath, Primitive, ReturnSuccess, Signature, SyntaxShape, UntaggedValue, Value,
};
use nu_source::{Tag, Tagged};

#[derive(Deserialize)]
pub struct Arguments {
    pub rest: Vec<ColumnPath>,
}

pub struct SubCommand;

#[async_trait]
impl WholeStreamCommand for SubCommand {
    fn name(&self) -> &str {
        "hash md5"
    }

    fn signature(&self) -> Signature {
        Signature::build("hash md5")
            .rest(
                SyntaxShape::ColumnPath,
                "optionally md5 encode data by column paths",
            )
    }

    fn usage(&self) -> &str {
        "md5 encode a value"
    }

    async fn run(&self, args: CommandArgs) -> Result<OutputStream, ShellError> {
        operate(args).await
    }

    fn examples(&self) -> Vec<Example> {
        vec![
            Example {
                description: "MD5 encode a string",
                example: "echo 'username:password' | hash md5",
                result: Some(vec![
                    UntaggedValue::string("dXNlcm5hbWU6cGFzc3dvcmQ=").into_untagged_value()
                ]),
            },
        ]
    }
}

async fn operate(args: CommandArgs) -> Result<OutputStream, ShellError> {
    let name_tag = &args.call_info.name_tag.clone();

    let (
        Arguments {
            rest,
        },
        input,
    ) = args.process().await?;

    let column_paths: Vec<_> = rest;

    Ok(input
        .map(move |v| {
            if column_paths.is_empty() {
                ReturnSuccess::value(action(&v, v.tag())?)
            } else {
                let mut ret = v;

                for path in &column_paths {
                    ret = ret.swap_data_by_column_path(
                        path,
                        Box::new(move |old| action(old, old.tag())),
                    )?;
                }

                ReturnSuccess::value(ret)
            }
        })
        .to_output_stream())
}

fn action(
    input: &Value,
    tag: impl Into<Tag>,
) -> Result<Value, ShellError> {
    match &input.value {
        UntaggedValue::Primitive(Primitive::Line(s))
        | UntaggedValue::Primitive(Primitive::String(s)) => {
            Ok(UntaggedValue::string(md5::compute(&s)).into_value(tag))
        }
        other => {
            let got = format!("got {}", other.type_name());
            Err(ShellError::labeled_error(
                "value is not a string or a binary",
                got,
                tag.into().span,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{action};
    use nu_protocol::UntaggedValue;
    use nu_source::Tag;
    use nu_test_support::value::string;

    #[test]
    fn md5_encode_a_string() {
        let word = string("username:password");
        let expected = UntaggedValue::string("dXNlcm5hbWU6cGFzc3dvcmQ=").into_untagged_value();

        let actual = action(
            &word,
            &Base64Config {
                character_set: "standard".to_string(),
                action_type: ActionType::Encode,
            },
            Tag::unknown(),
        )
        .unwrap();
        assert_eq!(actual, expected);
    }
}
