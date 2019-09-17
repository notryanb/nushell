use crate::parser::TokenNode;
use derive_new::new;

#[derive(Debug, new)]
pub struct TokensIterator<'a> {
    tokens: &'a [TokenNode],
    skip_ws: bool,
    #[new(default)]
    index: usize,
    #[new(default)]
    seen: indexmap::IndexSet<usize>,
}

#[derive(Debug)]
pub struct Peeked<'a, 'b> {
    pub(crate) node: &'a TokenNode,
    iterator: &'b mut TokensIterator<'a>,
    commit_point: usize,
}

impl<'a, 'b> Peeked<'a, 'b> {
    pub(crate) fn commit(self) -> (&'a mut TokensIterator<'b>, &'a TokenNode) {
        let mut iterator = self.iterator;
        let node = self.node;
        iterator.commit(self.commit_point);
        (iterator, node)
    }
}

impl<'a> TokensIterator<'a> {
    pub fn remove(&mut self, position: usize) {
        self.seen.insert(position);
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn at_end(&self) -> bool {
        let mut tokens = self.clone();

        tokens.next().is_none()
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

    pub fn clone(&self) -> TokensIterator<'a> {
        TokensIterator {
            tokens: self.tokens,
            index: self.index,
            seen: self.seen.clone(),
            skip_ws: self.skip_ws,
        }
    }

    // Get the next token, not including whitespace
    pub fn next_non_ws(&mut self) -> Option<&TokenNode> {
        next(self, true)
    }

    // Peek the next token, not including whitespace
    pub fn peek_non_ws<'b>(&'b mut self) -> Option<Peeked<'a, 'b>> {
        let mut tokens = self.clone();

        next(&mut tokens, true).map(|node| Peeked {
            node,
            iterator: self,
            commit_point: tokens.index,
        })
    }

    // Get the next token, including whitespace
    pub fn next_any(&mut self) -> Option<&TokenNode> {
        next(self, false)
    }

    // Peek the next token, including whitespace
    pub fn peek_any<'b>(&'b mut self) -> Option<Peeked<'a, 'b>> {
        let mut tokens = self.clone();

        next(&mut tokens, false).map(|node| Peeked {
            node,
            iterator: self,
            commit_point: tokens.index,
        })
    }

    fn commit(&mut self, commit_point: usize) {
        self.index = commit_point;
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
