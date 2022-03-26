use std::{iter::FusedIterator, mem, ops::Add};

#[derive(Debug, Clone)]
pub struct LoopBuffer<T, const N: usize> {
    buffer: [T; N],
    head: usize,
}

impl<T, const N: usize> LoopBuffer<T, N> {
    pub fn new(buffer: [T; N]) -> Self {
        Self { buffer, head: 0 }
    }

    pub fn push(&mut self, item: T) -> T {
        debug_assert!(self.head < N);
        let old_item = mem::replace(unsafe { self.buffer.get_unchecked_mut(self.head) }, item);
        self.head = self.head.add(1).rem_euclid(N);
        old_item
    }

    pub fn into_array(mut self) -> [T; N] {
        self.buffer.rotate_left(self.head);
        self.buffer
    }
}

#[derive(Debug)]
pub struct BufferIterator<'a, I: Iterator, const N: usize> {
    iter: I,
    buffer: &'a mut LoopBuffer<I::Item, N>,
}

impl<'a, I: Iterator, const N: usize> BufferIterator<'a, I, N> {
    pub fn new(iter: I, buffer: &'a mut LoopBuffer<I::Item, N>) -> Self {
        Self { iter, buffer }
    }
}

impl<'a, I: Iterator, const N: usize> Iterator for BufferIterator<'a, I, N> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|item| self.buffer.push(item))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter
            .fold(init, move |accum, item| f(accum, self.buffer.push(item)))
    }
}

impl<'a, I: FusedIterator, const N: usize> FusedIterator for BufferIterator<'a, I, N> {}
