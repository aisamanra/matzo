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

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub span: Span,
    pub file: FileRef,
}

#[derive(Debug)]
pub enum FileSource {
    File(String),
    Repl(u32),
}

pub struct File {
    pub source: FileSource,
    pub content: String,
}

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

    pub fn add_file(&mut self, path: String, content: String) -> FileRef {
        self.add_file_from_source(FileSource::File(path), content)
    }

    pub fn add_repl_line(&mut self, content: String) -> FileRef {
        let source = FileSource::Repl(self.last_repl_line);
        self.last_repl_line += 1;
        self.add_file_from_source(source, content)
    }

    fn add_file_from_source(&mut self, source: FileSource, content: String) -> FileRef {
        let idx = self.files.len();
        self.files.push(File { source, content });
        FileRef { idx }
    }

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
