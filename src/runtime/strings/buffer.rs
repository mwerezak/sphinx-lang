/*
    This is a port of the inline string data structure from flexstr <https://github.com/nu11ptr/flexstr>
    The original implementation was written by Scott Meeuwsen <https://github.com/nu11ptr> and can be found here:
    <https://github.com/nu11ptr/flexstr/blob/be94d3647bad6a626aa40c1e20290a71c1ee8a74/src/inline.rs>
*/

use core::ptr;
use core::str;
use core::fmt;
use core::mem::{self, MaybeUninit};
use core::borrow::Borrow;


// TODO evaluate what size makes the most sense for the StringValue struct

/// Using this inline capacity will result in a type with the same memory size as a builtin `String`
// pub const STRING_SIZED_INLINE: usize = mem::size_of::<String>() - 2;

#[derive(Clone, Copy)]
pub struct StrBuffer<const N: usize> {
    data: [MaybeUninit<u8>; N],
    len: u8,
}

// Conversion from other string types
impl<'s, const N: usize> TryFrom<&'s String> for StrBuffer<N> {
    type Error = &'s String;
    #[inline]
    fn try_from(value: &'s String) -> Result<Self, Self::Error> {
        Self::try_from(value)
    }
}

impl<'s, const N: usize> TryFrom<&'s str> for StrBuffer<N> {
    type Error = &'s str;
    #[inline]
    fn try_from(value: &'s str) -> Result<Self, Self::Error> {
        Self::try_from(value)
    }
}

impl<const N: usize> StrBuffer<N> {
    /// Create a new empty `StrBuffer`
    #[inline]
    pub fn new() -> Self {
        Self {
            len: 0,
            data: [MaybeUninit::<u8>::uninit(); N]
        }
    }
    
    /// Attempts to return a new `StrBuffer` if the source string is short enough to be copied.
    /// If not, the source is returned as the error.
    #[inline]
    fn try_from<S: AsRef<str>>(s: S) -> Result<Self, S> {
        let s_ref = s.as_ref();

        if s_ref.len() <= Self::capacity() {
            unsafe { Ok(Self::new_unchecked(s_ref)) }
        } else {
            Err(s)
        }
    }
    
    #[inline]
    unsafe fn new_unchecked(s: &str) -> Self {
        // SAFETY: This is safe because while uninitialized to start, we copy the the str contents
        // over the top. We check to ensure it is not too long in `try_new` and don't call this
        // function directly. The copy is restrained to the length of the str.

        // Declare array, but keep uninitialized (we will overwrite momentarily)
        let mut data = [MaybeUninit::<u8>::uninit(); N];
        // Copy contents of &str to our data buffer
        ptr::copy_nonoverlapping(s.as_ptr(), data.as_mut_ptr().cast::<u8>(), s.len());

        Self {
            len: s.len() as u8,
            data,
        }
    }
    
    #[inline]
    fn from_array(data: [MaybeUninit<u8>; N], len: u8) -> Self {
        Self { data, len }
    }

    /// Returns the capacity of this inline string
    #[inline]
    pub fn capacity() -> usize { N }

    /// Returns the length of this `InlineFlexStr` in bytes
    #[inline]
    pub fn len(&self) -> usize { self.len as usize }

    /// Return true if the inline string is empty else false
    #[inline]
    pub fn is_empty(&self) -> bool { self.len == 0 }

    pub fn try_push(&mut self, ch: char) -> Result<(), ()> {
        let mut buf = [0u8; 4];
        self.try_push_str(ch.encode_utf8(&mut buf))
    }

    /// Attempts to concatenate the `&str` if there is room. It returns true if it is able to do so.
    #[inline]
    pub fn try_push_str<S: AsRef<str>>(&mut self, s: S) -> Result<(), ()> {
        let s_ref = s.as_ref();
        
        if self.len() + s_ref.len() <= Self::capacity() {
            // Point to the location directly after our string
            let data = self.data[self.len as usize..].as_mut_ptr().cast::<u8>();

            unsafe {
                // SAFETY: We know the buffer is large enough and that the location is not overlapping
                // this one (we know that because we have ownership of one of them)
                // Copy contents of &str to our data buffer
                ptr::copy_nonoverlapping(s_ref.as_ptr(), data, s_ref.len());
            }
            self.len += s_ref.len() as u8;
            Ok(())
        } else {
            Err(())
        }
    }
}


impl<const N: usize> core::ops::Deref for StrBuffer<N> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        let data = &self.data[..self.len()];

        unsafe {
            // SAFETY: The contents are always obtained from a valid UTF8 str, so they must be valid
            // Additionally, we clamp the size of the slice passed to be no longer than our str length
            let data = &*(data as *const [mem::MaybeUninit<u8>] as *const [u8]);
            str::from_utf8_unchecked(data)
        }
    }
}

impl<const N: usize> AsRef<str> for StrBuffer<N> {
    fn as_ref(&self) -> &str { &*self }
}

impl<const N: usize> Borrow<str> for StrBuffer<N> {
    fn borrow(&self) -> &str { &*self }
}

impl<const N: usize> fmt::Debug for StrBuffer<N> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Debug>::fmt(self, f)
    }
}

impl<const N: usize> fmt::Display for StrBuffer<N> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(self, f)
    }
}





#[cfg(test)]
mod tests {
    use crate::runtime::strings::buffer::StrBuffer;

    #[test]
    fn empty() {
        let lit = "";
        let s: StrBuffer<22> = lit.try_into().expect("bad inline str");
        assert_eq!(&*s, lit);
        assert_eq!(s.len(), lit.len())
    }

    #[test]
    fn good_init() {
        let lit = "inline";
        let s: StrBuffer<22> = lit.try_into().expect("bad inline str");
        assert_eq!(&*s, lit);
        assert_eq!(s.len(), lit.len())
    }

    #[test]
    fn bad_init() {
        let lit = "This is way too long to be an inline string!!!";
        let s = <StrBuffer<22>>::try_from(lit).unwrap_err();
        assert_eq!(s, lit);
        assert_eq!(s.len(), lit.len())
    }

    #[test]
    fn good_concat() {
        let lit = "Inline";
        let lit2 = " me";
        let mut s = <StrBuffer<22>>::try_from(lit).expect("bad inline str");
        assert!(s.try_concat(lit2));
        assert_eq!(&*s, lit.to_string() + lit2);
    }

    #[test]
    fn bad_concat() {
        let lit = "This is";
        let lit2 = " way too long to be an inline string!!!";
        let mut s = <StrBuffer<22>>::try_from(lit).expect("bad inline str");
        assert!(!s.try_concat(lit2));
        assert_eq!(&*s, lit);
    }
}