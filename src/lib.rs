//! Reusable error type

// TODO: Add better type structure to root error messages
//
// It might be good to add better structure to error messages.
// right now the only data attached to an error is a string. It may be good to add other data. for
// instance OutOfRange could hold OutOfRange{val: T, min: T, max: T} so that it could be used by
// code more effectively. Won't do much for error messages besides lessening typing repetition
//
// The is some error codes are fairly general and it would be difficult to determine what the
// fields should be. For instance what should go with a NotFound error. It is too vague in
// application.
//
// That said there is value in even just some errors having more structure. So I'll probably just
// add typing piecemeal

use std::{
    cell::BorrowMutError,
    fmt, io,
    num::{ParseFloatError, ParseIntError},
    sync::PoisonError,
};

use conversions::*;

#[cfg(feature = "wgpu")]
use wgpu::{RequestDeviceError, SurfaceError};

/// propogate with throw! macro
/// example:
/// throw!(File::open("foo").context("opening foo"))
///
/// instead of: File::open("foo").context("opening foo")?
///
/// will add linenumber information to the error
#[macro_export]
macro_rules! throw {
    ($e:expr, notrace causes $code:expr, $($arg:tt)+) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(reerror::Error::Cause(
                    $code,
                    format!($($arg)+),
                    std::boxed::Box::new(e.into()),
                ))
            }
        }
    };

    ($e:expr, causes $code:expr, $($arg:tt)+) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(reerror::Error::Trace(
                    concat!(file!(), ":", line!()),
                    std::boxed::Box::new(reerror::Error::Cause(
                        $code,
                        format!($($arg)+),
                        std::boxed::Box::new(e.into()),
                    )),
                ))
            }
        }
    };

    ($e:expr, notrace $($arg:tt)+) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(reerror::Error::Context(
                    format!($($arg)+),
                    std::boxed::Box::new(e.into()),
                ))
            }
        }
    };

    ($e:expr, if none $code:expr, $($arg:tt)+) => {
        match $e {
            Some(x) => x,
            None => return Err(reerror::Error::Message($code, format!($($arg)+))),
        }
    };

    ($e:expr, $($arg:tt)+) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(reerror::Error::Trace(
                    concat!(file!(), ":", line!()),
                    std::boxed::Box::new(reerror::Error::Context(
                        format!($($arg)+),
                        std::boxed::Box::new(e.into()),
                    )),
                ))
            }
        }
    };

    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                return Err(reerror::Error::Trace(
                    concat!(file!(), ":", line!()),
                    std::boxed::Box::new(e.into()),
                ))
            }
        }
    };
}

#[derive(Debug, Copy, Clone)]
pub enum StatusCode {
    Cancelled,
    Unknown,
    InvalidArgument,
    DeadlineExceeded,
    NotFound,
    AlreadyExists,
    PermissionDenied,
    ResourceExhausted,
    FailedPrecondition,
    Aborted,
    OutOfRange,
    Unimplemented,
    Internal,
    Unavailable,
    DataLoss,
    Unauthenticated,
}

impl fmt::Display for StatusCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            StatusCode::Cancelled => write!(f, "Cancelled"),
            StatusCode::Unknown => write!(f, "Unknown"),
            StatusCode::InvalidArgument => write!(f, "Invalid Argument"),
            StatusCode::DeadlineExceeded => write!(f, "Deadline Exceeded"),
            StatusCode::NotFound => write!(f, "Not Found"),
            StatusCode::AlreadyExists => write!(f, "Already Exists"),
            StatusCode::PermissionDenied => write!(f, "Permission Denied"),
            StatusCode::ResourceExhausted => write!(f, "Resource Exhausted"),
            StatusCode::FailedPrecondition => write!(f, "Failed Precondition"),
            StatusCode::Aborted => write!(f, "Aborted"),
            StatusCode::OutOfRange => write!(f, "Out of Range"),
            StatusCode::Unimplemented => write!(f, "Unimplemented"),
            StatusCode::Internal => write!(f, "Internal"),
            StatusCode::Unavailable => write!(f, "Unavailable"),
            StatusCode::DataLoss => write!(f, "Data Loss"),
            StatusCode::Unauthenticated => write!(f, "Unauthenticated"),
        }
    }
}

pub trait Context {
    type Out;
    fn context<S: Into<String>>(self, msg: S) -> Self::Out;
    fn from_cause<S: Into<String>>(self, code: StatusCode, msg: S) -> Self::Out;

    fn with_context<F: FnOnce() -> String>(self, f: F) -> Self::Out;
}

#[derive(Debug, Clone)]
pub enum Error {
    Message(StatusCode, String),
    Context(String, Box<Error>),
    Cause(StatusCode, String, Box<Error>),
    Trace(&'static str, Box<Error>),
}

impl Error {
    pub fn new<S: Into<String>>(code: StatusCode, msg: S) -> Self {
        Self::Message(code, msg.into())
    }

    pub fn code(&self) -> StatusCode {
        match self {
            Error::Message(code, _) => *code,
            Error::Context(_, err) => err.code(),
            Error::Cause(code, _, _) => *code,
            Error::Trace(_, err) => err.code(),
        }
    }
}

/// Contains mostly methods to be used with Result::map_err
/// to simplfiy one off conversions of an error type into this type
pub mod conversions {
    use std::fmt::Display;

    use crate::{Error, StatusCode};

    pub fn cancelled<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Cancelled, e.to_string())
    }

    pub fn unknown<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Unknown, e.to_string())
    }

    pub fn invalid_argument<E: Display>(e: E) -> Error {
        Error::new(StatusCode::InvalidArgument, e.to_string())
    }

    pub fn deadline_exceeded<E: Display>(e: E) -> Error {
        Error::new(StatusCode::DeadlineExceeded, e.to_string())
    }

    pub fn not_found<E: Display>(e: E) -> Error {
        Error::new(StatusCode::NotFound, e.to_string())
    }

    pub fn already_exists<E: Display>(e: E) -> Error {
        Error::new(StatusCode::AlreadyExists, e.to_string())
    }

    pub fn permission_denied<E: Display>(e: E) -> Error {
        Error::new(StatusCode::PermissionDenied, e.to_string())
    }

    pub fn resource_exhausted<E: Display>(e: E) -> Error {
        Error::new(StatusCode::ResourceExhausted, e.to_string())
    }

    pub fn failed_precondition<E: Display>(e: E) -> Error {
        Error::new(StatusCode::FailedPrecondition, e.to_string())
    }

    pub fn aborted<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Aborted, e.to_string())
    }

    pub fn out_of_range<E: Display>(e: E) -> Error {
        Error::new(StatusCode::OutOfRange, e.to_string())
    }

    pub fn unimplemented<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Unimplemented, e.to_string())
    }

    pub fn internal<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Internal, e.to_string())
    }

    pub fn unavailable<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Unavailable, e.to_string())
    }

    pub fn data_loss<E: Display>(e: E) -> Error {
        Error::new(StatusCode::DataLoss, e.to_string())
    }

    pub fn unauthenticated<E: Display>(e: E) -> Error {
        Error::new(StatusCode::Unauthenticated, e.to_string())
    }
}

impl Context for Error {
    type Out = Self;
    fn context<S: Into<String>>(self, msg: S) -> Self::Out {
        Error::Context(msg.into(), Box::new(self))
    }

    fn with_context<F: FnOnce() -> String>(self, f: F) -> Self::Out {
        self.context((f)())
    }

    fn from_cause<S: Into<String>>(self, code: StatusCode, msg: S) -> Self::Out {
        Error::Cause(code, msg.into(), Box::new(self))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), fmt::Error> {
        match self {
            Error::Message(code, msg) => write!(f, "{code}: {msg}"),
            Error::Context(msg, err) => {
                write!(f, "{err}\n\tcontext: {msg}")
            }
            Error::Cause(code, msg, err) => write!(f, "{code}: {msg}\n\tcause: {err}"),
            Error::Trace(msg, err) => write!(f, "{err}\n\tat: {msg}"),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

impl<T, E: Into<Error>> Context for std::result::Result<T, E> {
    type Out = Result<T>;
    fn context<S: Into<String>>(self, msg: S) -> Self::Out {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(Error::Context(msg.into(), Box::new(e.into()))),
        }
    }

    fn with_context<F: FnOnce() -> String>(self, f: F) -> Self::Out {
        match self {
            Ok(a) => Ok(a),
            Err(e) => Err(Error::Context((f)(), Box::new(e.into()))),
        }
    }

    fn from_cause<S: Into<String>>(self, code: StatusCode, msg: S) -> Self::Out {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(Error::Cause(code, msg.into(), Box::new(e.into()))),
        }
    }
}

impl<T> Context for Option<T> {
    type Out = Result<T>;
    fn context<S: Into<String>>(self, msg: S) -> Self::Out {
        match self {
            Some(t) => Ok(t),
            None => Err(Error::Context(msg.into(), Box::new(unknown("None value")))),
        }
    }

    fn with_context<F: FnOnce() -> String>(self, f: F) -> Self::Out {
        match self {
            Some(t) => Ok(t),
            None => Err(Error::Context((f)(), Box::new(unknown("None value")))),
        }
    }

    fn from_cause<S: Into<String>>(self, code: StatusCode, msg: S) -> Self::Out {
        match self {
            Some(t) => Ok(t),
            None => Err(Error::Cause(
                code,
                msg.into(),
                Box::new(unknown("None value")),
            )),
        }
    }
}

// --- Everything below here is conversion methods ---
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        let kind = match e.kind() {
            io::ErrorKind::NotFound => StatusCode::NotFound,
            io::ErrorKind::PermissionDenied => StatusCode::PermissionDenied,
            io::ErrorKind::AlreadyExists => StatusCode::AlreadyExists,
            io::ErrorKind::TimedOut => {
                return Error::new(StatusCode::DeadlineExceeded, e.to_string())
                    .from_cause(StatusCode::Cancelled, "")
            }
            _ => StatusCode::Unknown,
        };
        Self::new(kind, e.to_string())
    }
}

impl From<ParseIntError> for Error {
    fn from(e: ParseIntError) -> Self {
        invalid_argument(e)
    }
}

impl From<ParseFloatError> for Error {
    fn from(e: ParseFloatError) -> Self {
        invalid_argument(e)
    }
}

#[cfg(feature = "image")]
impl From<image::ImageError> for Error {
    fn from(e: image::ImageError) -> Self {
        use image::ImageError;
        match e {
            ImageError::Encoding(e) => internal(e),
            ImageError::Decoding(e) => internal(e),
            ImageError::Parameter(e) => invalid_argument(e),
            ImageError::Limits(e) => resource_exhausted(e),
            ImageError::Unsupported(e) => unimplemented(e),
            ImageError::IoError(e) => Error::from(e),
        }
    }
}

#[cfg(feature = "wgpu")]
impl From<SurfaceError> for Error {
    fn from(e: SurfaceError) -> Self {
        let kind = match e {
            SurfaceError::Lost => StatusCode::Unavailable,
            SurfaceError::Timeout => StatusCode::DeadlineExceeded,
            SurfaceError::Outdated => StatusCode::Unavailable,
            SurfaceError::OutOfMemory => StatusCode::ResourceExhausted,
        };
        Error::new(kind, e.to_string())
    }
}

#[cfg(feature = "wgpu")]
impl From<RequestDeviceError> for Error {
    fn from(e: RequestDeviceError) -> Self {
        internal(e)
    }
}

impl<T> From<PoisonError<T>> for Error {
    fn from(e: PoisonError<T>) -> Self {
        internal(e)
    }
}

impl From<BorrowMutError> for Error {
    fn from(e: BorrowMutError) -> Self {
        internal(e)
    }
}
