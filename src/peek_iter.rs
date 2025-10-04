use core::mem;

pub struct PeekIter<T, I> {
    iter: I,
    current: Option<T>,
    next: Option<T>,
}

impl<T, I: Iterator<Item = T>> PeekIter<T, I> {
    pub fn new(mut iter: I) -> Self {
        let current = iter.next();
        let next = iter.next();
        Self {
            iter,
            current,
            next,
        }
    }

    pub const fn current(&self) -> Option<&T> {
        self.current.as_ref()
    }

    pub const fn peek(&self) -> Option<&T> {
        self.next.as_ref()
    }

    pub fn advance(&mut self) -> Option<T> {
        self.next()
    }
}

impl<T: Copy, I: Iterator<Item = T>> PeekIter<T, I> {
    pub const fn current_copied(&self) -> Option<T> {
        self.current
    }

    pub const fn peek_copied(&self) -> Option<T> {
        self.next
    }
}

impl<T, I: Iterator<Item = T>> Iterator for PeekIter<T, I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        mem::replace(
            &mut self.current,
            mem::replace(&mut self.next, self.iter.next()),
        )
    }
}
