use core::fmt;

pub fn application<H, T>(head: &H, tail: &[T], f: &mut fmt::Formatter) -> fmt::Result
where
    H: fmt::Display,
    T: fmt::Display,
{
    let parens = !tail.is_empty();
    if parens {
        write!(f, "(")?;
    };
    write!(f, "{}", head)?;
    for t in tail {
        write!(f, " {}", t)?;
    }
    if parens {
        write!(f, ")")?;
    };
    Ok(())
}
