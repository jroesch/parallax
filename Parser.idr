module Parser

import Prelude.Algebra

data IResult i t r = Fail i (List String) (List String)
                   | Partial (i -> IResult i t r)
                   | Done i r

instance (Show i, Show r) => Show (IResult i t r) where
    show (Fail t stk msg) =
      unwords ["Fail", show t, show stk, show msg]
    show (Partial _) = "Partial _"
    show (Done t r)  = unwords ["Done", show t, show r]

instance Functor (IResult i t) where
    map _ (Fail t stk msg) = Fail t stk msg
    map f (Partial k)      = Partial (\i => map f (k i))
    map f (Done t r)       = Done t (f r)

data More = Complete 
          | Incomplete

instance Semigroup More where
    Complete <+> _ = Complete
    _        <+> m = m

instance Monoid More where
  neutral = Incomplete  

record Pos : Type where
  MkPos : (fromPos : Nat) -> Pos

Failure : Type -> Type -> Type -> Type 
Failure i t r = t -> Pos -> More -> (List String) -> String -> IResult i t r

Success : Type -> Type -> Type -> Type -> Type
Success i t a r = t -> Pos -> More -> a -> IResult i t r

record Parser : Type -> Type -> Type -> Type where
  MkParser : (runParser : (r : Type ** t -> Pos -> More -> Failure i t r -> Success i t a r -> IResult i t r)) -> Parser i t a

typeOf : {A : Type} -> (a : A) -> Type
typeOf {A} _ = A

instance Functor (Parser i t) where
    map _ x = ?whooo

mResultType : {a : Type} -> {b : Type} -> (a -> Parser i t b) -> Type
mResultType {a} {b} _ = b

pbind : Parser i t a -> (a -> Parser i t b) -> Parser i t b
pbind = ?bind

instance Applicative (Parser i t) where
    pure v = MkParser (typeOf v ** kont)
        where kont : t -> Pos -> More -> Failure i t r -> Success i t a r -> IResult i t r
              kont t pos more fail succ = succ t pos more v

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


