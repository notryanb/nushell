mod base64_;
mod md5_;
mod command;

pub use base64_::SubCommand as HashBase64;
pub use md5_::SubCommand as HashMd5;
pub use command::Command as Hash;
