use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub(crate) line: u16,
    pub(crate) col: u16,
}

pub struct Span<T> {
    pub(crate) start: Pos,
    pub(crate) end: Pos,
    pub(crate) item: T,
}

impl<T: fmt::Debug> fmt::Debug for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("item", &self.item)
            .finish()
    }
}
