// Inspiration:
// - [Rust Source](https://github.com/rust-lang/rust/blob/1.45.2/src/librustc_span/lib.rs)
// - [Museun - Neta](https://github.com/museun/neta/blob/04_scanner/src/span.rs)

use std::convert::From;
use std::fmt;
use std::ops::{Add, AddAssign, Index};

pub trait Pos {
    fn from_usize(n: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn from_u32(n: u32) -> Self;
    fn to_u32(&self) -> u32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytePos(pub u32);

impl Pos for BytePos {
    #[inline(always)]
    fn from_usize(n: usize) -> Self {
        Self(n as u32)
    }

    #[inline(always)]
    fn to_usize(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    fn from_u32(n: u32) -> Self {
        Self(n)
    }

    #[inline(always)]
    fn to_u32(&self) -> u32 {
        self.0
    }
}

impl From<u32> for BytePos {
    fn from(value: u32) -> Self {
        Self::from_u32(value)
    }
}

impl From<usize> for BytePos {
    fn from(value: usize) -> Self {
        Self::from_usize(value)
    }
}

impl Add for BytePos {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Add<u32> for BytePos {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl AddAssign<u32> for BytePos {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CharPos(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub low: BytePos,
    pub high: BytePos,
}

impl Span {
    #[inline]
    pub fn new(mut low: BytePos, mut high: BytePos) -> Self {
        if low > high {
            std::mem::swap(&mut low, &mut high);
        }

        Self { low, high }
    }

    #[inline]
    pub fn low(self) -> BytePos {
        self.low
    }

    #[inline]
    pub fn high(self) -> BytePos {
        self.low
    }

    #[inline]
    pub fn with_low(self, low: BytePos) -> Self {
        Self::new(low, self.high)
    }

    #[inline]
    pub fn with_high(self, high: BytePos) -> Self {
        Self::new(self.low, high)
    }

    #[inline]
    pub fn span<T>(self, item: T) -> Spanned<T> {
        Spanned { span: self, item }
    }

    /// Returns a `Span` which encloses both `self` and `other`.
    pub fn union(self, other: Self) -> Self {
        Self::new(
            std::cmp::min(self.low, other.low),
            std::cmp::max(self.high, other.high),
        )
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[index.low.to_usize()..index.high.to_usize()]
    }
}

pub struct Spanned<T> {
    pub span: Span,
    pub item: T,
}

impl<T> Spanned<T> {
    pub fn into_item(self) -> T {
        self.item
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Spanned")
            .field("span", &self.span)
            .field("item", &self.item)
            .finish()
    }
}

impl<T> Index<Spanned<T>> for str {
    type Output = str;

    fn index(&self, index: Spanned<T>) -> &Self::Output {
        &self[index.span]
    }
}
