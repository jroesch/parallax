module Parallax.ByteString.Core

import Data.ByteString
import Parallax.Internal
import Parallax.ByteString.Buffer
import Parallax.Types

Parser : Type -> Type
Parser a = Parallax.Types.Parser String Buffer a

Result : Type -> Type
Result a = IResult String a

Failure : Type -> Type
Failure r = Parallax.Types.Failure String Buffer r

Success : Type -> Type -> Type
Success a r = Parallax.Types.Success String Buffer a r

advance : Nat -> Parser ()
advance = ?advance

ensureSuspended : Int -> Buffer -> Pos -> More
                -> Failure r
                -> Success ByteString r
                -> Result r
ensureSuspended = ?ensureSuspended

ensure : Nat -> Parser ByteString
ensure = ?ensure

peekBits8 : Parser (Maybe Bits8)
peekBits8 = MkParser $ \t, buf, (MkPos pos_), more, f, succ =>
                if pos_ < Buffer.length buf
                then succ buf (MkPos pos_) more (Just $ Buffer.unsafeIndex buf pos_)
                else if more == Complete
                then succ buf (MkPos pos_) more Nothing
                else
                  let succ' = ?here -- (\t', pos', more' => succ t' pos' more' (Just $ Buffer.unsafeIndex buf pos_))
                      -- fail' = ?there -- (\t', pos', more' => succ t' pos' more' Nothing)
                    in prompt t pos more ?fail ?succ -- fail' succ'

peekBits8' : Parser Bits8
peekBits8' = ?peekBits8'

satisfy : (Bits8 -> Bool) -> Parser Bits8
satisfy p = do
  h <- peekBits8'
  if p h
     then return h
     else fail "satisfy"

skip : (Bits8 -> Bool) -> Parser ()
skip p = do
  h <- peekBits8'
  if p h
     then advance 1
     else fail "skip"

{- satisfyWith : {a : Type} -> (Bits8 -> a) -> (a -> Bool) -> Parser a
satisfyWith {x} f p = do
  h <- peekBits8'
  let c : x = f h
  if p c
     then advance 1 >> return c
     else fail "satisfyWith" -}

takeWith : Nat -> (ByteString -> Bool) -> Parser ByteString
takeWith n p = do
  s <- ensure n
  if p s then (advance n >>= \_ => return s) else fail "takeWith"

take : Nat -> Parser ByteString
take n = takeWith n (const True)

-- note be careful about mixing constructor/type name
failK : Failure a
failK t (MkPos pos) more stack msg = Fail (Buffer.unsafeDrop pos t) stack msg

successK : Success a a
successK t (MkPos pos) more a = Done (Buffer.unsafeDrop pos t) a

parse : Parser a -> String -> Result a
parse p input = Parallax.Types.runParser p (buffer input) (MkPos 0) Incomplete failK successK
