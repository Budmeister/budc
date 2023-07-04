//! Author:     Brian Smith
//! Year:       2023

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Either<T, U> {
    This(T),
    That(U),
}
impl<T: std::fmt::Display, U: std::fmt::Display> std::fmt::Display for Either<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Either::This(t) => write!(f, "{}", t),
            Either::That(u) => write!(f, "{}", u),
        }
    }
}
impl<T: std::fmt::Display, U: std::fmt::Display> std::fmt::Debug for Either<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
