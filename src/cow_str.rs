use bumpalo::{Bump, collections::String};
use core::{fmt::Display, ops::Deref};

/// A specialized string type that is either an owned string (that exists on a bump),
/// or a string reference. Always changes to an owned string when mutated.
#[derive(Debug)]
pub enum Inner<'src, 'bump> {
    Borrowed(&'src str),
    Owned(String<'bump>),
}

impl<'src, 'bump> Inner<'src, 'bump> {
    pub const fn owned(str: String<'bump>) -> Self {
        Self::Owned(str)
    }

    pub const fn borrowed(s: &'src str) -> Self {
        Self::Borrowed(s)
    }

    pub const fn is_owned(&self) -> bool {
        matches!(self, Self::Owned(_))
    }

    pub const fn is_borrowed(&self) -> bool {
        !self.is_owned()
    }
}

#[derive(Debug)]
pub struct CowStr<'src, 'bump> {
    inner: Inner<'src, 'bump>,
    bump: &'bump Bump,
}

impl<'src, 'bump: 'src> CowStr<'src, 'bump> {
    pub const fn new(bump: &'bump Bump) -> Self {
        Self::borrowed("", bump)
    }

    pub const fn borrowed(str: &'src str, bump: &'bump Bump) -> Self {
        Self {
            inner: Inner::borrowed(str),
            bump,
        }
    }

    pub fn owned(s: String<'bump>) -> Self {
        let bump = s.bump();
        Self {
            inner: Inner::owned(s),
            bump,
        }
    }

    pub fn to_mut(&mut self) -> &mut String<'bump> {
        match self.inner {
            Inner::Owned(ref mut owned) => owned,
            Inner::Borrowed(brw) => {
                self.inner = Inner::owned(String::from_str_in(brw, self.bump));
                match self.inner {
                    Inner::Owned(ref mut own) => own,
                    _ => unreachable!(),
                }
            }
        }
    }

    pub fn to_borrowed(&self) -> &str {
        match self.inner {
            Inner::Borrowed(brw) => brw,
            Inner::Owned(ref own) => &*own,
        }
    }
}

impl<'src, 'bump> Deref for CowStr<'src, 'bump> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.to_borrowed()
    }
}

impl<'src, 'bump: 'src> Clone for CowStr<'src, 'bump> {
    fn clone(&self) -> Self {
        match self.inner {
            Inner::Owned(ref owned) => Self::owned(owned.clone()),
            Inner::Borrowed(borrow) => Self::borrowed(borrow, self.bump),
        }
    }
}

impl<'src, 'bump: 'src> AsRef<str> for CowStr<'src, 'bump> {
    fn as_ref(&self) -> &str {
        &*self
    }
}

impl Display for CowStr<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_borrowed())
    }
}

impl<'src, 'bump: 'src> From<String<'bump>> for CowStr<'src, 'bump> {
    fn from(value: String<'bump>) -> Self {
        Self::owned(value)
    }
}

#[macro_export]
macro_rules! cow_str {
    (in $bump:expr; $s:literal) => {
        $crate::cow_str::CowStr::borrowed($s, $bump)
    };

    (owned $e:expr) => {
        $crate::cow_str::CowStr::owned($e)
    };
}
