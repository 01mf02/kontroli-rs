use crate::Token;

pub struct Period<S, I> {
    next: Option<Token<S>>,
    iter: I,
}

impl<S, I> Period<S, I> {
    pub fn new(iter: I) -> Self {
        Self { iter, next: None }
    }
}

impl<S, I: Iterator<Item = Token<S>>> Iterator for Period<S, I> {
    type Item = Token<S>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.take() {
            return Some(next);
        }
        while let Some(token) = self.iter.next() {
            match token {
                Token::Space | Token::Comment(0) => (),
                Token::Dot => match self.iter.next() {
                    Some(Token::Space) | Some(Token::Comment(0)) | None => {
                        return Some(Token::Period);
                    }
                    Some(open @ Token::Comment(_)) => {
                        self.next = Some(open);
                        return Some(Token::Period);
                    }
                    Some(other) => {
                        self.next = Some(other);
                        return Some(Token::Dot);
                    }
                },
                _ => return Some(token),
            }
        }
        None
    }
}
