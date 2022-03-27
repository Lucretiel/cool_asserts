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

    pub fn bind_iterator<'a>(
        &'a mut self,
        iter: impl Iterator<Item = T> + 'a,
    ) -> impl Iterator<Item = T> + 'a {
        iter.map(move |item| self.push(item))
    }

    pub fn into_array(mut self) -> [T; N] {
        self.buffer.rotate_left(self.head);
        self.buffer
    }
}

#[test]
fn test_loop_buffer() {
    let data = [3, 4, 5, 6];
    let mut buffer = LoopBuffer::new(data);

    let iterated: Vec<i32> = buffer.bind_iterator(7..).take(11).collect();

    assert_eq!(iterated, [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]);
    assert_eq!(buffer.into_array(), [14, 15, 16, 17])
}

#[inline(always)]
#[doc(hidden)]
pub fn erase_type<I: Iterator + FusedIterator>(
    iter: I,
) -> impl Iterator<Item = I::Item> + FusedIterator {
    iter
}
