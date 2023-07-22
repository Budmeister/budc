use std::ops::Range;



pub enum BudErr {
    CompilerErr(CompilerErr),
    UserErr(UserErr),
}

impl BudErr {
    pub fn get_msg(&self) -> &str {
        match self {
            Self::CompilerErr(comp) => &comp.msg,
            Self::UserErr(user) => &user.msg,
        }
    }
    pub fn get_location(&self) -> Option<Range<usize>> {
        match self {
            Self::CompilerErr(comp) => comp.location,
            Self::UserErr(user) => user.location,
        }
    }
}

impl From<CompilerErr> for BudErr {
    fn from(value: CompilerErr) -> Self {
        Self::CompilerErr(value)
    }
}

impl From<UserErr> for BudErr {
    fn from(value: UserErr) -> Self {
        Self::UserErr(value)
    }
}

pub struct CompilerErr {
    pub msg: String,
    pub location: Option<Range<usize>>,
}

pub struct UserErr {
    pub msg: String,
    pub location: Option<Range<usize>>,
}

pub trait Ranged {
    fn get_range(&self) -> Range<usize>;
}

#[macro_export]
macro_rules! c_err_opt {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: $location.map(|r| r.to_owned()),
        }.into())
    };
}

#[macro_export]
macro_rules! u_err_opt {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: $location.map(|r| r.to_owned()),
        }.into())
    };
}

#[macro_export]
macro_rules! c_err {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: Some($location.to_owned()),
        }.into())
    };
    ($msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::CompilerErr {
            msg: format!($msg $(, $($arg)*)?),
            location: None,
        }.into())
    };
}

#[macro_export]
macro_rules! u_err {
    ($location:expr, $msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: Some($location.to_owned()),
        }.into())
    };
    ($msg:literal $(, $($arg:tt)*)?) => {
        Err($crate::error::UserErr {
            msg: format!($msg $(, $($arg)*)?),
            location: None,
        }.into())
    };
}