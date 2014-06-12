module Parallax.ByteString.Buffer

import Data.ByteString

record Buffer : Type where
  Buf : ByteString -> Buffer

instance Show Buffer where
    show _ = "a buffer"

instance Semigroup Buffer where
    (Buf b1) <+> (Buf b2) = Buf (b1 ++ b2)

instance Monoid Buffer where
    neutral = Buf ""
    
namespace Buffer
    unsafeDrop : Nat -> Buffer -> ByteString
    unsafeDrop Z b = ?rightCase
    unsafeDrop n b = ?leftCase
      {- if (length b) > 0
        then ?nonempty
        else Left ("trying to pull " ++ (show n) ++ " bytes") -}
    
    unsafeIndex : Buffer -> Nat -> Bits8
    unsafeIndex = ?unsafeIndex 

    length : Buffer -> Nat
    length (Buf bs) = length bs

    substring : Nat -> Nat -> Buffer -> ByteString
    substring s l (Buf bs) = ?substring

buffer : ByteString -> Buffer
buffer str = Buf str

unbuffer : Buffer -> ByteString
unbuffer (Buf str) = str
