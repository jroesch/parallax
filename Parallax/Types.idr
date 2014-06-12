module Parallax.Types

import Prelude.Algebra
import Data.ByteString
import Data.Text

data IResult i r = Fail i (List String) String
                 | Partial (i -> IResult i r)
                 | Done i r

instance (Show i, Show r) => Show (IResult i r) where
    show (Fail t stk msg) =
      unwords ["Fail", show t, show stk, show msg]
    show (Partial _) = "Partial _"
    show (Done t r)  = unwords ["Done", show t, show r]

instance Functor (IResult i) where
    map _ (Fail t stk msg) = Fail t stk msg
    map f (Partial k)      = Partial (\i => map f (k i))
    map f (Done t r)       = Done t (f r)

data More = Complete 
          | Incomplete

instance Eq More where
    Complete == Complete = True
    Incomplete == Incomplete = True
    _ == _ = False

instance Semigroup More where
    Complete <+> _ = Complete
    _        <+> m = m

instance Monoid More where
    neutral = Incomplete  

record Pos : Type where
    MkPos : (fromPos : Nat) -> Pos

data IsState s r = MkIsState

byteStringST : IsState ByteString Bits8
byteStringST = MkIsState

textST : IsState Text Char
textST = MkIsState

State : { auto p : IsState t b } -> (t : Type) -> State
State = ?state

Failure : Type -> Type -> Type -> Type 
Failure i t r = t -> Pos -> More -> (List String) -> String -> IResult i r

Success : Type -> Type -> Type -> Type -> Type
Success i t a r = t -> Pos -> More -> a -> IResult i r

IParser : Type -> Type -> Type -> Type -> Type
IParser i t a r = t -> Pos -> More -> Failure i t r -> Success i t a r -> IResult i r

data Parser : Type -> Type -> Type -> Type where
  MkParser : ((r : Type) -> IParser i t a r) -> Parser i t a

runParser : {r : Type} -> Parser i t a -> t -> Pos -> More -> Failure i t r -> Success i t a r -> IResult i r
runParser {r} (MkParser quant_parser) = quant_parser r

typeOf : {A : Type} -> (a : A) -> Type
typeOf {A} _ = A

{- What the fuck did I have to do here? -}
domain : {a : Type} -> {b : Type} -> (a -> b) -> Type
domain {a} {b} _ = a

codomain : {a : Type} -> {b : Type} -> (a -> b) -> Type
codomain {a} {b} _ = b

kont : (a -> b) -> IParser i t a r -> t -> Pos -> More -> Failure i t r -> Success i t b r -> IResult i r
kont f k t pos more fail suc = k t pos more fail (\t' => \pos' => \more' => \a => suc t' pos' more' (f a))

imap : (a -> b) -> IParser i t a r -> IParser i t b r
imap f k = kont f k

instance Functor (Parser i t) where
    map f (MkParser quant_parser) = MkParser (\r : Type => imap f (quant_parser r))

mResultType : {a : Type} -> {b : Type} -> (a -> Parser i t b) -> Type
mResultType {a} {b} _ = b

pbind : Parser i t a -> (a -> Parser i t b) -> Parser i t b
pbind = ?bind

fail : String -> Parser i t a
fail str = MkParser (\r : Type => \t => \pos => \more => \f => \_ => f t pos more Prelude.List.Nil ("Failed reading: " ++ str))

instance Applicative (Parser i t) where
    pure v = MkParser $ \r => \t => \pos => \more => \fail => \succ => succ t pos more v
    f <$> x = pbind f (\f' => pbind x (\x' => pure (f' x')))

instance Monad (Parser i t) where
    m >>= f = pbind m f

{- instance Applicative (Parser i t) where
    pure = return
    (<*>) = do
      b <- d
      a <- e
      return (b a)

instance (Semigroup t) => Semigroup (Parser i t a) where
    (<+>) = plus

instance (Monoid t) => Monoid (Parser i t a) where
neutral = fail "empty" -}

Chunk : Type -> Type
Chunk ByteString = Bits8
Chunk Text = Char
