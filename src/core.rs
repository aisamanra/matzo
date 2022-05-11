#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileRef {
    pub idx: usize,
}

/// A location in a source file
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn empty() -> Span {
        Span {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    pub fn exists(&self) -> bool {
        self.start != u32::MAX && self.end != u32::MAX
    }
}
