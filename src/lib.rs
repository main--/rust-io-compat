#![cfg_attr(not(feature = "std"), no_std)]
extern crate alloc;

pub struct IoCompat<T> { inner: T }

impl<T> IoCompat<T> {
    pub fn into_inner(self) -> T { self.inner.into() }
}
impl<T: Inner> AsRef<T::T> for IoCompat<T> {
    fn as_ref(&self) -> &T::T {
        self.inner.get()
    }
}
impl<T: Inner> AsMut<T::T> for IoCompat<T> {
    fn as_mut(&mut self) -> &mut T::T {
        self.inner.get_mut()
    }
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum ErrorKind {
    Interrupted,
    InvalidData,
    InvalidInput,
    UnexpectedEof,
    WriteZero,
    Other,
    Uncategorized,
}

macro_rules! error_kind_row {
    ($x:expr, $($ele:ident),*) => {
        match $x {
            $(From::$ele => To::$ele,)*
            _ => To::Other,
        }
    }
}
macro_rules! error_kind_rows {
    ($x:expr) => {
        error_kind_row!(
            $x,
            Interrupted,
            InvalidData,
            InvalidInput,
            UnexpectedEof,
            WriteZero
        )
    }
}
macro_rules! error_kind {
    ($errorkind:ty) => {
        impl From<$errorkind> for crate::ErrorKind {
            fn from(x: $errorkind) -> Self {
                use $errorkind as From;
                use crate::ErrorKind as To;
                error_kind_rows!(x)
            }
        }
        impl Into<$errorkind> for crate::ErrorKind {
            fn into(self) -> $errorkind {
                use crate::ErrorKind as From;
                use $errorkind as To;
                error_kind_rows!(self)
            }
        }
    };
}


#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    #[cfg(feature = "alloc")]
    #[allow(dead_code)]
    message: alloc::string::String,
}
impl Error {
    #[allow(unused_variables)]
    pub fn new(kind: ErrorKind, x: impl core::fmt::Debug) -> Self {
        Self {
            kind,
            #[cfg(feature = "alloc")]
            message: alloc::format!("{x:?}"),
        }
    }
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}
type Result<T> = core::result::Result<T, Error>;


pub trait Inner {
    type T;
    fn into(self) -> Self::T;
    fn get(&self) -> &Self::T;
    fn get_mut(&mut self) -> &mut Self::T;
}

trait BridgeRead {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;

    #[cfg(feature = "alloc")]
    fn read_to_end(&mut self, buf: &mut alloc::vec::Vec<u8>) -> Result<usize> {
        let mut b = [0u8; 4096];
        let mut read = 0;
        loop {
            let r = self.read(&mut b)?;
            if r == 0 {
                break;
            }
            read += r;
            buf.extend_from_slice(&b[..r]);
        }

        Ok(read)
    }

    #[cfg(feature = "alloc")]
    fn read_to_string(&mut self, buf: &mut alloc::string::String) -> Result<usize> {
        let mut vec = core::mem::replace(buf, alloc::string::String::new()).into_bytes();
        let prev_len = vec.len();
        let read = self.read_to_end(&mut vec)?;
        match alloc::string::String::from_utf8(vec) {
            Ok(x) => {
                *buf = x;
                Ok(read)
            }
            Err(e) => {
                let mut vec = e.into_bytes();
                vec.truncate(prev_len);
                *buf = alloc::string::String::from_utf8(vec).expect("These bytes were valid UTF-8 when you gave them to me, so it better be valid now");
                Err(crate::Error::new(crate::ErrorKind::InvalidData, "data was not valid utf-8"))
            }
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> Result<()>;
}

#[derive(Copy, PartialEq, Eq, Clone, Debug)]
enum SeekFrom {
    Start(u64),
    End(i64),
    Current(i64),
}

trait BridgeSeek {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64>;
    fn rewind(&mut self) -> Result<()> {
        self.seek(SeekFrom::Start(0)).map(|_| ())
    }
    fn stream_position(&mut self) -> Result<u64> {
        self.seek(SeekFrom::Current(0))
    }
}

pub trait BridgeWrite {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
    fn write_all(&mut self, buf: &[u8]) -> Result<()>;
    fn write_fmt(&mut self, fmt: core::fmt::Arguments<'_>) -> Result<()> {
        // impl copied from libstd
        use core::fmt;
        // Create a shim which translates a Write to a fmt::Write and saves
        // off I/O errors. instead of discarding them
        struct Adapter<'a, T: ?Sized + 'a> {
            inner: &'a mut T,
            error: Result<()>,
        }

        impl<T: BridgeWrite + ?Sized> fmt::Write for Adapter<'_, T> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.inner.write_all(s.as_bytes()) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }

        let mut output = Adapter { inner: self, error: Ok(()) };
        match fmt::write(&mut output, fmt) {
            Ok(()) => Ok(()),
            Err(..) => {
                // check if the error came from the underlying `Write` or not
                if output.error.is_err() {
                    output.error
                } else {
                    Err(Error::new(ErrorKind::Other, "formatting error"))
                }
            }
        }
    }
}

macro_rules! impl_seek {
    (core2, $error:ty, $seek:path, $seekfrom:ty) => {
        impl<T: $seek> BridgeSeek for FromThis<T> {
            fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
                self.inner.seek(pos.into()).map_err(Into::into)
            }
        }
        impl<T: BridgeSeek> $seek for IoCompat<T> {
            fn seek(&mut self, pos: $seekfrom) -> core::result::Result<u64, $error> {
                self.inner.seek(pos.into()).map_err(Into::into)
            }
        }
    };
    ($mod: ident, $error:ty, $seek:path, $seekfrom: ty) => {
        impl<T: $seek> BridgeSeek for FromThis<T> {
            fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
                self.inner.seek(pos.into()).map_err(Into::into)
            }
            fn rewind(&mut self) -> Result<()> {
                self.inner.rewind().map_err(Into::into)
            }
            fn stream_position(&mut self) -> Result<u64> {
                self.inner.stream_position().map_err(Into::into)
            }
        }
        impl<T: BridgeSeek> $seek for IoCompat<T> {
            fn seek(&mut self, pos: $seekfrom) -> core::result::Result<u64, $error> {
                self.inner.seek(pos.into()).map_err(Into::into)
            }
            fn rewind(&mut self) -> core::result::Result<(), $error> {
                self.inner.rewind().map_err(Into::into)
            }
            fn stream_position(&mut self) -> core::result::Result<u64, $error> {
                self.inner.stream_position().map_err(Into::into)
            }
        }
    };
}

macro_rules! read_to_end {
    (fatfs, $dir:ident) => {};
    ($mod: ident, $dir: ident) => {
        #[cfg(feature = "alloc")]
        fn read_to_end(&mut self, buf: &mut alloc::vec::Vec<u8>) -> Result<usize> {
            self.inner.read_to_end(buf).map_err(Into::into)
        }
    };
}
macro_rules! read_to_string {
    (core2, $dir:ident) => {};
    (fatfs, $dir:ident) => {};
    ($mod: ident, $dir: ident) => {
        #[cfg(feature = "alloc")]
        fn read_to_string(&mut self, buf: &mut alloc::string::String) -> Result<usize> {
            self.inner.read_to_string(buf).map_err(Into::into)
        }
    };
}

macro_rules! read_body {
    ($mod: ident, $dir: ident) => {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            self.inner.read(buf).map_err(Into::into)
        }

        read_to_end!($mod, $dir);
        read_to_string!($mod, $dir);

        fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
            self.inner.read_exact(buf).map_err(Into::into)
        }
    };
}

macro_rules! write_fmt {
    (fatfs) => {};
    ($mod: ident) => {
        fn write_fmt(&mut self, fmt: core::fmt::Arguments<'_>) -> Result<()> {
            self.inner.write_fmt(fmt).map_err(Into::into)
        }
    };
}
macro_rules! write_body {
    ($mod: ident) => {
        fn write(&mut self, buf: &[u8]) -> Result<usize> {
            self.inner.write(buf).map_err(Into::into)
        }
        fn flush(&mut self) -> Result<()> {
            self.inner.flush().map_err(Into::into)
        }
        fn write_all(&mut self, buf: &[u8]) -> Result<()> {
            self.inner.write_all(buf).map_err(Into::into)
        }
        write_fmt!($mod);
    };
}

macro_rules! seek_from {
    ($seekfrom:ty) => {
        impl From<$seekfrom> for SeekFrom {
            fn from(x: $seekfrom) -> Self {
                use $seekfrom as Theirs;
                match x {
                    Theirs::Start(x) => SeekFrom::Start(x),
                    Theirs::Current(x) => SeekFrom::Current(x),
                    Theirs::End(x) => SeekFrom::End(x),
                }
            }
        }
        impl Into<$seekfrom> for SeekFrom {
            fn into(self) -> $seekfrom {
                use $seekfrom as Theirs;
                match self {
                    SeekFrom::Start(x) => Theirs::Start(x),
                    SeekFrom::Current(x) => Theirs::Current(x),
                    SeekFrom::End(x) => Theirs::End(x),
                }
            }
        }
    };
}

macro_rules! from_this {
    () => {
        pub struct FromThis<T> { inner: T }
        impl<T> Inner for FromThis<T> {
            type T = T;

            fn into(self) -> Self::T {
                self.inner
            }

            fn get(&self) -> &T {
                &self.inner
            }

            fn get_mut(&mut self) -> &mut T {
                &mut self.inner
            }
        }
    };
}

macro_rules! into_error {
    (core2, $error:ty, $errorkind:ty, $self:ident) => {
        <$error>::new($self.kind().into(), "sadly, core2 does not allow propagating dynamic error data so here is a 'static string for you :)")
    };
    ($mod:ident, $error:ty, $errorkind:ty, $self:ident) => {
        {
            #[cfg(feature = "alloc")]
            {
                <$error>::new($self.kind().into(), $self.message.as_str())
            }
            #[cfg(not(feature = "alloc"))]
            {
                <$error>::from(Into::<$errorkind>::into($self.kind()))
            }
        }
    };
}
macro_rules! compat_impl {
    ($mod:ident, $feature:literal, $from:ident, $read_compat:ident, $error:ty, $errorkind:ty, $read:path, $seek:path, $seekfrom:ty, $write:path) => {
        #[cfg(feature = $feature)]
        mod $mod {
            use crate::{BridgeRead, BridgeSeek, BridgeWrite, SeekFrom, Inner, IoCompat, Error, Result};
            impl From<$error> for Error {
                fn from(e: $error) -> Self {
                    Error::new(e.kind().into(), e)
                }
            }
            impl Into<$error> for Error {
                fn into(self) -> $error {
                    into_error!($mod, $error, $errorkind, self)
                }
            }

            error_kind!($errorkind);

            pub trait ReadCompat {
                fn compat(self) -> IoCompat<FromThis<Self>> where Self: Sized { IoCompat { inner: FromThis { inner: self } } }
            }
            impl<T: $read> ReadCompat for T {}
            from_this!();


            impl<T: $read> BridgeRead for FromThis<T> {
                read_body!($mod, inner);
            }
            impl<T: $write> BridgeWrite for FromThis<T> {
                write_body!($mod);
            }
            mod for_compat {
                use super::*;
                type Result<T> = core::result::Result<T, $error>;
                impl<T: BridgeRead> $read for IoCompat<T> {
                    read_body!($mod, outer);
                }
                impl<T: BridgeWrite> $write for IoCompat<T> {
                    write_body!($mod);
                }
            }

            seek_from!($seekfrom);

            impl_seek!($mod, $error, $seek, $seekfrom);
        }
        #[cfg(feature = $feature)]
        from_mod!($mod, $from, $read_compat);
    };
}

macro_rules! from_mod {
    ($mod:ident, $this: ident, $read_compat:ident) => {
        pub use crate::$mod::{FromThis as $this, ReadCompat as $read_compat};
    };
}

cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        compat_impl!(std, "std", FromStd, StdReadCompat, std::io::Error, std::io::ErrorKind, std::io::Read, std::io::Seek, std::io::SeekFrom, std::io::Write);
        from_mod!(std, FromAcid, AcidReadCompat);
        from_mod!(std, FromCore2, Core2ReadCompat);
        from_mod!(std, FromFatfs, FatfsReadCompat);
    } else {
        compat_impl!(acid_io, "acid_io", FromAcid, AcidReadCompat, acid_io::Error, acid_io::ErrorKind, acid_io::Read, acid_io::Seek, acid_io::SeekFrom, acid_io::Write);
        compat_impl!(core2, "core2", FromCore2, CoreReadCompat, core2::io::Error, core2::io::ErrorKind, core2::io::Read, core2::io::Seek, core2::io::SeekFrom, core2::io::Write);
        #[cfg(feature = "fatfs")]
        mod fatfs {
            use crate::{BridgeRead, BridgeSeek, BridgeWrite, SeekFrom, Inner, IoCompat, Error, Result};

            impl<T> fatfs::IoBase for IoCompat<T> {
                type Error = crate::Error;
            }
            impl fatfs::IoError for crate::Error {
                fn is_interrupted(&self) -> bool {
                    self.kind == crate::ErrorKind::Interrupted
                }

                fn new_unexpected_eof_error() -> Self {
                    crate::Error::new(crate::ErrorKind::UnexpectedEof, "fatfs: unexpected eof")
                }

                fn new_write_zero_error() -> Self {
                    crate::Error::new(crate::ErrorKind::WriteZero, "fatfs: write zero")
                }
            }

            impl From<fatfs::Error<Error>> for Error {
                fn from(e: fatfs::Error<Error>) -> Self {
                    match e {
                        fatfs::Error::Io(e) => e,
                        _ => Error::new(crate::ErrorKind::Other, e),
                    }
                }
            }

            from_this!();


            impl<T: fatfs::Read<Error=fatfs::Error<Error>>> BridgeRead for FromThis<T> {
                read_body!(fatfs, inner);
            }
            impl<T: fatfs::Write<Error=fatfs::Error<Error>>> BridgeWrite for FromThis<T> {
                write_body!(fatfs);
            }

            mod for_compat {
                use super::*;
                impl<T: BridgeRead> fatfs::Read for IoCompat<T> {
                    read_body!(fatfs, outer);
                }
                impl<T: BridgeWrite> fatfs::Write for IoCompat<T> {
                    write_body!(fatfs);
                }
            }

            seek_from!(fatfs::SeekFrom);
            impl<T: fatfs::Seek<Error=fatfs::Error<Error>>> BridgeSeek for FromThis<T> {
                fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
                    self.inner.seek(pos.into()).map_err(Into::into)
                }
            }
            impl<T: BridgeSeek> fatfs::Seek for IoCompat<T> {
                fn seek(&mut self, pos: fatfs::SeekFrom) -> core::result::Result<u64, Error> {
                    self.inner.seek(pos.into()).map_err(Into::into)
                }
            }
        }
        #[cfg(feature = "fatfs")]
        pub use crate::fatfs::FromThis as FromFatfs;
    }
}
