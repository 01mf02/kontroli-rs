use nom::error::{VerboseError, VerboseErrorKind};
use std::fmt;

/// obtain the line number, the following input and the column number
/// of a substring in a string
pub fn line_column<'a>(input: &'a [u8], substring: &'a [u8]) -> (usize, &'a [u8], usize) {
    use nom::{AsBytes, Offset};

    let offset = input.offset(substring);

    let prefix = &input.as_bytes()[..offset];

    // Count the number of newlines in the first `offset` bytes of input
    let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

    // Find the line that includes the subslice:
    // Find the *last* newline before the substring starts
    let line_begin = prefix
        .iter()
        .rev()
        .position(|&b| b == b'\n')
        .map(|pos| offset - pos)
        .unwrap_or(0);

    // Find the string after that newline
    let line = &input[line_begin..];

    // The (1-indexed) column number is the offset of our substring into that string
    let column_number = line.offset(substring) + 1;

    (line_number, line, column_number)
}

fn write_error_empty(f: &mut fmt::Formatter, kind: &VerboseErrorKind) -> fmt::Result {
    match kind {
        VerboseErrorKind::Char(c) => write!(f, "expected '{}', got empty input", c),
        VerboseErrorKind::Context(s) => write!(f, "in {}, got empty input", s),
        VerboseErrorKind::Nom(e) => write!(f, "in {:?}, got empty input", e),
    }
}

fn write_error_line_column(
    f: &mut fmt::Formatter,
    kind: &VerboseErrorKind,
    line_number: usize,
    line: String,
    column_number: usize,
    substring: String,
) -> fmt::Result {
    match kind {
        VerboseErrorKind::Char(c) => {
            write!(
                f,
                "at line {line_number}:\n\
               {line}\n\
               {caret:>column$}\n\
               expected '{expected}', ",
                line_number = line_number,
                line = line,
                caret = '^',
                column = column_number,
                expected = c
            )?;
            if let Some(actual) = substring.chars().next() {
                write!(f, "found {}", actual)
            } else {
                write!(f, "got end of input")
            }
        }
        VerboseErrorKind::Context(s) => write!(
            f,
            "at line {line_number}, in {context}:\n\
             {line}\n\
             {caret:>column$}\n\n",
            line_number = line_number,
            context = s,
            line = line,
            caret = '^',
            column = column_number,
        ),
        VerboseErrorKind::Nom(e) => write!(
            f,
            "at line {line_number}, in {nom_err:?}:\n\
             {line}\n\
             {caret:>column$}\n\n",
            line_number = line_number,
            nom_err = e,
            line = line,
            caret = '^',
            column = column_number,
        ),
    }
}

/// transforms a `VerboseError` into a trace with input position information
pub fn write_error(
    f: &mut fmt::Formatter,
    input: &[u8],
    e: VerboseError<&[u8]>,
) -> fmt::Result {
    use std::io::BufRead;

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        write!(f, "{}: ", i)?;
        if input.is_empty() {
            write_error_empty(f, kind)?;
        } else {
            let (line_number, line, column_number) = line_column(input, substring);

            // FIXME: The following commands fail if input contains invalid UTF-8!

            // Find the full line after that newline
            let line = line
                .lines()
                .next()
                .map(|l| l.unwrap())
                .unwrap_or_else(|| String::from_utf8(line.to_vec()).unwrap());

            let substring = String::from_utf8(substring.to_vec()).unwrap();
            write_error_line_column(f, kind, line_number, line, column_number, substring)?;
        }
        write!(f, "\n\n")?;
    }
    Ok(())
}
