use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Span {
    // Start byte offset (inclusive)
    pub start: u32,

    // End byte offset (exclusive)
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        debug_assert!(
            start <= end,
            "Start must be less than or equal to end (start: {}, end: {})",
            start,
            end
        );
        Span { start, end }
    }
}

impl Display for Span {
    // TODO: Very simple display of byte offsets - a more elaborate version would actually
    // show lines and columns.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end || self.start + 1 == self.end {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}..{}", self.start, self.end)
        }
    }
}

pub fn substring(source: &str, span: Span) -> &str {
    let start = span.start as usize;
    let end = span.end as usize;
    assert!(start <= source.len() && end <= source.len());
    return &source[start..end];
}
