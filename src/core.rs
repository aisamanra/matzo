/// A location in a source file
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Create a span which does not refer to any specific part of the
    /// file. (We should avoid using this if at all possible, but it
    /// does help in a few places.)
    pub fn empty() -> Span {
        Span {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    /// Returns `true` if this is a `Span::empty()`, and `false`
    /// otherwise.
    pub fn exists(&self) -> bool {
        self.start != u32::MAX && self.end != u32::MAX
    }
}

/// A `Span` and a `FileRef` bundled together are a `Loc` which can
/// unambiguously reference a chunk of file
#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub span: Span,
    pub file: FileRef,
}

/// This represents where we got this specific fragment from: either a
/// file or a REPL interaction
#[derive(Debug)]
pub enum FileSource {
    File(String),
    Repl(u32),
}

/// A `FileRef` represents a loaded file _or_ a line in the REPL.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileRef {
    pub idx: usize,
}

/// A `File` is a bundled string and the place that string comes
/// from. Not only do we use this as the source for parsing later on,
/// we also use it to produce nicely-formatted error messages
/// elsewhere.
pub struct File {
    pub source: FileSource,
    pub content: String,
}

/// The `FileTable` keeps track of all the files/REPL lines we've
/// loaded so far. All source is kept in memory. (This shouldn't be a
/// problem for the kinds of tiny programs Matzo is intended for.)
pub struct FileTable {
    files: Vec<File>,
    last_repl_line: u32,
}

impl FileTable {
    pub fn new() -> FileTable {
        FileTable {
            files: Vec::new(),
            last_repl_line: 0,
        }
    }

    /// Add a file at the supplied path to the file table
    pub fn add_file(&mut self, path: String, content: String) -> FileRef {
        self.add_file_from_source(FileSource::File(path), content)
    }

    /// Add a line from the REPL as a new file in the file table. (We
    /// keep track of how many REPL lines we've seen so far
    /// internally, so this will be given an unambiguous index and we
    /// can refer back to the line in error messages.)
    pub fn add_repl_line(&mut self, content: String) -> FileRef {
        let source = FileSource::Repl(self.last_repl_line);
        self.last_repl_line += 1;
        self.add_file_from_source(source, content)
    }

    /// Add a `File` to the internal table and return the `FileRef`
    fn add_file_from_source(&mut self, source: FileSource, content: String) -> FileRef {
        let idx = self.files.len();
        self.files.push(File { source, content });
        FileRef { idx }
    }

    /// Given a `Loc`, produce the line that the `Loc` came from with
    /// a second line that points to the span within the line,
    /// specifically for error messages. If the `Loc` contains an
    /// empty span, then this will produce an empty string instead.
    pub fn get_line(&self, loc: Loc) -> String {
        if !loc.span.exists() {
            return String::new();
        }

        let mut line_number = 1;
        let mut start_of_line = 0;
        let mut end_of_line = None;
        let file = &self.files[loc.file.idx];
        let span = loc.span;
        let src = &file.content;

        // this is... maybe not the most efficient way of doing this,
        // given that we end up walking basically the whole string,
        // but again, this is a dinky language for producing strings,
        // and we only call this on error, so maybe we can afford to
        // be O(n) in the size of the kinds of small files we care
        // about here. It's fine. It's fine! It's fine.
        for (i, ch) in src.char_indices() {
            if ch == '\n' {
                if i < span.start as usize {
                    line_number += 1;
                    start_of_line = i + 1;
                }
                if i >= span.end as usize && end_of_line.is_none() {
                    end_of_line = Some(i);
                }
            }
        }
        let end_of_line = end_of_line.unwrap_or(src.len());

        // If this comes from the REPL, then we should add the REPL
        // offset to the line number.
        if let FileSource::Repl(offset) = file.source {
            line_number += offset;
        }

        let mut result = format!("{:3} |", line_number);
        result.push_str(&src[start_of_line..end_of_line]);
        result.push_str("\n     ");
        for _ in start_of_line..(span.start as usize) {
            result.push(' ');
        }
        for _ in span.start..span.end {
            result.push('^');
        }
        result
    }
}

impl Default for FileTable {
    fn default() -> Self {
        FileTable::new()
    }
}
