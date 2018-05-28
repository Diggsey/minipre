use std::collections::BTreeMap;
use std::io::{BufRead, Write, self};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Context {
    defs: BTreeMap<String, String>
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Syntax {
        line: u32,
        msg: &'static str
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::Io(ref e) => e.fmt(f),
            &Error::Syntax { msg, line } => write!(f, "{} on line {}", msg, line)
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::Io(ref e) => e.description(),
            &Error::Syntax { msg, .. } => msg
        }
    }
    fn cause(&self) -> Option<&std::error::Error> {
        match self {
            &Error::Io(ref e) => Some(e),
            &Error::Syntax { .. } => None
        }
    }
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::Io(other)
    }
}

impl Context {
    pub fn new() -> Self {
        Context {
            defs: BTreeMap::new()
        }
    }
    pub fn define<N: Into<String>, V: Into<String>>(&mut self, name: N, value: V) -> &mut Self {
        self.defs.insert(name.into(), value.into());
        self
    }
    fn skip_whitespace(&self, expr: &mut &str) {
        *expr = expr.trim_left();
    }
    fn eval_term(&self, expr: &mut &str, line: u32) -> Result<bool, Error> {
        self.skip_whitespace(expr);
        
        let index = expr.find(|c: char| !c.is_ascii_alphanumeric() && c != '_').unwrap_or(expr.len());
        let term = &expr[0..index];
        *expr = &expr[index..];

        if term.chars().next().ok_or_else(|| Error::Syntax {
            line,
            msg: "Expected term, found nothing"
        })?.is_digit(10) {
            Ok(term == "1")
        } else {
            Ok(self.defs.get(term).ok_or_else(|| Error::Syntax {
                line,
                msg: "Undefined identifier"
            })? == "1")
        }
    }
    fn eval_unary(&self, expr: &mut &str, line: u32) -> Result<bool, Error> {
        let mut negate = false;
        self.skip_whitespace(expr);
        while expr.starts_with("!") {
            *expr = &expr[1..];
            negate = !negate;
            self.skip_whitespace(expr);
        }

        Ok(negate ^ self.eval_term(expr, line)?)
    }
    fn eval_eq(&self, expr: &mut &str, line: u32) -> Result<bool, Error> {
        let mut result = self.eval_unary(expr, line)?;
        self.skip_whitespace(expr);
        while expr.starts_with("==") {
            *expr = &expr[2..];
            result ^= !self.eval_unary(expr, line)?;
            self.skip_whitespace(expr);
        }
        Ok(result)
    }
    fn evaluate(&self, mut expr: &str, line: u32) -> Result<bool, Error> {
        let result = self.eval_eq(&mut expr, line)?;
        self.skip_whitespace(&mut expr);
        if !expr.is_empty() {
            return Err(Error::Syntax {
                line,
                msg: "Expected end-of-line"
            })
        }
        Ok(result)
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum State {
    // A condition already matched, skip remaining clauses
    Skip,
    // Condition has not yet matched, evaluate remaining clauses
    Inactive,
    // Condition currently matches, pass through input
    Active,
}

pub fn process_str(input: &str, context: &mut Context) -> Result<String, Error> {
    let mut output = Vec::new();
    process(input.as_bytes(), &mut output, context)?;
    Ok(String::from_utf8(output).expect("Input was utf8, so output should be too..."))
}

pub fn process<I: BufRead, O: Write>(mut input: I, mut output: O, context: &mut Context) -> Result<(), Error> {
    let mut buf = String::new();
    let mut stack = Vec::new();
    let mut state = State::Active;
    let mut line = 0;

    while input.read_line(&mut buf)? > 0 {
        line += 1;
        {
            let substr = buf.trim();
            if substr.starts_with("#") {
                let mut parts = substr.splitn(2, "//").next().unwrap().splitn(2, " ");
                let name = parts.next().unwrap();
                let maybe_expr = parts.next().map(|s| s.trim()).filter(|s| !s.is_empty());

                match name {
                    "#if" => {
                        let expr = maybe_expr.ok_or_else(|| Error::Syntax {
                            line,
                            msg: "Expected expression after `#if`"
                        })?;
                        stack.push(state);
                        if state == State::Active {
                            if !context.evaluate(expr, line)? {
                                state = State::Inactive;
                            }
                        } else {
                            state = State::Skip;
                        }
                    },
                    "#elif" => {
                        let expr = maybe_expr.ok_or_else(|| Error::Syntax {
                            line,
                            msg: "Expected expression after `#elif`"
                        })?;
                        if state == State::Inactive {
                            if context.evaluate(expr, line)? {
                                state = State::Active;
                            }
                        } else {
                            state = State::Skip;
                        }
                    },
                    "#else" => {
                        if maybe_expr.is_some() {
                            return Err(Error::Syntax {
                                line,
                                msg: "Unexpected expression after `#else`"
                            })
                        }
                        if state == State::Inactive {
                            state = State::Active;
                        } else {
                            state = State::Skip;
                        }
                    },
                    "#endif" => {
                        if maybe_expr.is_some() {
                            return Err(Error::Syntax {
                                line,
                                msg: "Unexpected expression after `#else`"
                            })
                        }
                        state = stack.pop().ok_or_else(|| Error::Syntax {
                            line,
                            msg: "Unexpected `#endif` with no matching `#if`"
                        })?;
                    },
                    _ => {
                        return Err(Error::Syntax {
                            line,
                            msg: "Unrecognised preprocessor directive"
                        });
                    }
                }
            } else if state == State::Active {
                output.write_all(buf.as_bytes())?;
            }
        }
        buf.clear();
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pass_through() {
        assert_eq!(&process_str("
            some
            multiline
            text
            with # symbols
        ", &mut Context::new()).unwrap(), "
            some
            multiline
            text
            with # symbols
        ");
    }

    #[test]
    fn variable() {
        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "1")).unwrap(), "
            some
            multiline
            text
            with # symbols
        ");
    }

    #[test]
    fn constant() {
        assert_eq!(&process_str("
            some
            #if 0
            multiline
            text
            #endif
            with # symbols
        ", &mut Context::new()).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if 1
            multiline
            text
            #endif
            with # symbols
        ", &mut Context::new()).unwrap(), "
            some
            multiline
            text
            with # symbols
        ");
    }

    #[test]
    fn negation() {
        assert_eq!(&process_str("
            some
            #if !FOO
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "1")).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if !FOO
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            multiline
            text
            with # symbols
        ");
    }

    #[test]
    fn else_() {
        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #else
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            text
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #else
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "1")).unwrap(), "
            some
            multiline
            with # symbols
        ");
    }

    #[test]
    fn elif() {
        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #elif 1
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            text
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #elif 1
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "1")).unwrap(), "
            some
            multiline
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #elif 0
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #elif 0
            text
            #else
            with # symbols
            #endif
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO
            multiline
            #elif 1
            text
            #else
            with # symbols
            #endif
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            text
        ");
    }

    #[test]
    fn equality() {
        assert_eq!(&process_str("
            some
            #if FOO == 1
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            with # symbols
        ");

        assert_eq!(&process_str("
            some
            #if FOO == 0
            multiline
            text
            #endif
            with # symbols
        ", Context::new().define("FOO", "0")).unwrap(), "
            some
            multiline
            text
            with # symbols
        ");
    }

}
