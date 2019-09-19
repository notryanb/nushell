use crate::errors::ShellError;
use crate::parser::TokenNode;
use derive_new::new;

#[derive(Debug, new)]
pub struct TokensIterator<'a> {
    tokens: &'a [TokenNode],
    origin: uuid::Uuid,
    skip_ws: bool,
    #[new(default)]
    index: usize,
    #[new(default)]
    seen: indexmap::IndexSet<usize>,
}

#[derive(Debug)]
pub struct Peeked<'content, 'me> {
    pub(crate) node: Option<&'content TokenNode>,
    iterator: &'me mut TokensIterator<'content>,
    from: usize,
    to: usize,
}

impl<'content, 'me> Peeked<'content, 'me> {
    pub fn commit(self) -> Option<&'content TokenNode> {
        let Peeked {
            node,
            iterator,
            from,
            to,
        } = self;

        let node = node?;
        iterator.commit(from, to);
        Some(node)
    }

    pub fn not_eof(
        self,
        expected: impl Into<String>,
    ) -> Result<PeekedNode<'content, 'me>, ShellError> {
        match self.node {
            None => Err(ShellError::unexpected_eof(expected, self.iterator.origin)),
            Some(node) => Ok(PeekedNode {
                node,
                iterator: self.iterator,
                from: self.from,
                to: self.to,
            }),
        }
    }

    pub fn type_error(self, expected: impl Into<String>) -> ShellError {
        peek_error(self.node, self.iterator.origin, expected)
    }
}

#[derive(Debug)]
pub struct PeekedNode<'content, 'me> {
    pub(crate) node: &'content TokenNode,
    iterator: &'me mut TokensIterator<'content>,
    from: usize,
    to: usize,
}

impl<'content, 'me> PeekedNode<'content, 'me> {
    pub fn commit(self) -> &'content TokenNode {
        let PeekedNode {
            node,
            iterator,
            from,
            to,
        } = self;

        iterator.commit(from, to);
        node
    }

    pub fn type_error(self, expected: impl Into<String>) -> ShellError {
        peek_error(Some(self.node), self.iterator.origin, expected)
    }
}

pub fn peek_error(
    node: Option<&TokenNode>,
    origin: uuid::Uuid,
    expected: impl Into<String>,
) -> ShellError {
    match node {
        None => ShellError::unexpected_eof(expected, origin),
        Some(node) => ShellError::type_error(expected, node.tagged_type_name()),
    }
}

impl<'content> TokensIterator<'content> {
    #[cfg(test)]
    pub fn all(tokens: &'content [TokenNode], origin: uuid::Uuid) -> TokensIterator<'content> {
        TokensIterator::new(tokens, origin, false)
    }

    pub fn remove(&mut self, position: usize) {
        self.seen.insert(position);
    }

    pub fn at_end(&self) -> bool {
        peek(self, self.skip_ws).is_none()
    }

    pub fn advance(&mut self) {
        self.seen.insert(self.index);
        self.index += 1;
    }

    pub fn extract<T>(&mut self, f: impl Fn(&TokenNode) -> Option<T>) -> Option<(usize, T)> {
        for (i, item) in self.tokens.iter().enumerate() {
            if self.seen.contains(&i) {
                continue;
            }

            match f(item) {
                None => {
                    continue;
                }
                Some(value) => {
                    self.seen.insert(i);
                    return Some((i, value));
                }
            }
        }

        None
    }

    pub fn move_to(&mut self, pos: usize) {
        self.index = pos;
    }

    pub fn restart(&mut self) {
        self.index = 0;
    }

    pub fn clone(&self) -> TokensIterator<'content> {
        TokensIterator {
            tokens: self.tokens,
            origin: self.origin,
            index: self.index,
            seen: self.seen.clone(),
            skip_ws: self.skip_ws,
        }
    }

    // Get the next token, not including whitespace
    pub fn next_non_ws(&mut self) -> Option<&TokenNode> {
        let peeked = start_next(self, true);
        peeked.commit()
    }

    // Peek the next token, not including whitespace
    pub fn peek_non_ws<'me>(&'me mut self) -> Peeked<'content, 'me> {
        start_next(self, false)
    }

    // Get the next token, including whitespace
    pub fn next_any(&mut self) -> Option<&TokenNode> {
        let peeked = start_next(self, false);
        peeked.commit()
    }

    // Peek the next token, including whitespace
    pub fn peek_any<'me>(&'me mut self) -> Peeked<'content, 'me> {
        start_next(self, false)
    }

    fn commit(&mut self, from: usize, to: usize) {
        for index in from..to {
            self.seen.insert(index);
        }

        self.index = to;
    }

    pub fn debug_remaining(&self) -> Vec<TokenNode> {
        let mut tokens = self.clone();
        tokens.restart();
        tokens.cloned().collect()
    }

    pub fn is_empty(&self) -> bool {
        // This will behave correctly regardless of the `skip_ws` flag
        let mut tokens = self.clone();
        tokens.next().is_none()
    }
}

impl<'a> Iterator for TokensIterator<'a> {
    type Item = &'a TokenNode;

    fn next(&mut self) -> Option<&'a TokenNode> {
        next(self, self.skip_ws)
    }
}

fn peek<'content, 'me>(
    iterator: &TokensIterator<'content>,
    skip_ws: bool,
) -> Option<&'content TokenNode> {
    let from = iterator.index;
    let mut to = iterator.index;

    loop {
        if to >= iterator.tokens.len() {
            return None;
        }

        if iterator.seen.contains(&to) {
            to += 1;
            continue;
        }

        if to >= iterator.tokens.len() {
            return None;
        }

        let node = &iterator.tokens[to];

        match node {
            TokenNode::Whitespace(_) if skip_ws => {
                to += 1;
            }
            other => {
                to += 1;
                return Some(node);
            }
        }
    }
}

fn start_next<'content, 'me>(
    iterator: &'me mut TokensIterator<'content>,
    skip_ws: bool,
) -> Peeked<'content, 'me> {
    let from = iterator.index;
    let mut to = iterator.index;

    loop {
        if to >= iterator.tokens.len() {
            return Peeked {
                node: None,
                iterator,
                from,
                to,
            };
        }

        if iterator.seen.contains(&to) {
            to += 1;
            continue;
        }

        if to >= iterator.tokens.len() {
            return Peeked {
                node: None,
                iterator,
                from,
                to,
            };
        }

        let node = &iterator.tokens[to];

        match node {
            TokenNode::Whitespace(_) if skip_ws => {
                to += 1;
            }
            other => {
                to += 1;
                return Peeked {
                    node: Some(node),
                    iterator,
                    from,
                    to,
                };
            }
        }
    }
}

fn next<'a>(iterator: &mut TokensIterator<'a>, skip_ws: bool) -> Option<&'a TokenNode> {
    loop {
        if iterator.index >= iterator.tokens.len() {
            return None;
        }

        if iterator.seen.contains(&iterator.index) {
            iterator.advance();
            continue;
        }

        if iterator.index >= iterator.tokens.len() {
            return None;
        }

        match &iterator.tokens[iterator.index] {
            TokenNode::Whitespace(_) if skip_ws => {
                iterator.advance();
            }
            other => {
                iterator.advance();
                return Some(other);
            }
        }
    }
}
