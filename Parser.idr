module Parsing

class Monad m => MonadPlus (m : Type -> Type) where
    mzero : m a
    mplus : m a -> m a -> m a

data Result e a = Failed e | Parsed a

instance Functor (Result e) where
  map _ (Failed e) = Failed e
  map f (Parsed x) = Parsed (f x)

instance Applicative (Result e) where
    pure x = Parsed x

    (Failed e) <$> _ = Failed e
    _ <$> (Failed e) = Failed e
    (Parsed f) <$> (Parsed x) = Parsed (f x)
    
data Parser a = MkParser (String -> (String, Result String a))

instance Functor Parser where
    map g (MkParser f) = MkParser $ \input =>
        let (rest, r) = f input in
            (rest, map g r)

instance Applicative Parser where
    pure x = MkParser $ \input => (input, Parsed x)

    (MkParser f) <$> (MkParser g) = MkParser $ \input =>
        let (r, f') = f input in
        let (r', x) = g r
        in (r', f' <$> x)
         
instance Monad Parser where
  (MkParser f) >>= g = MkParser $ \input =>
    case f input of
        (rest, Failed e) => (rest, Failed e)
        (rest, Parsed x) => 
            let (MkParser h) = g x 
            in h rest

choice : Parser a -> Parser a -> Parser a
choice (MkParser f) (MkParser g) = MkParser $ \input =>
    case f input of
        (_, Failed _) => g input
        result => result

instance MonadPlus Parser where
    mzero = MkParser $ \input => (input, Failed "ParserZero: no parsing to be performed.")
    mplus = choice

instance Alternative Parser where
    empty = mzero
    m1 <|> m2 = mplus m1 m2

satisfy : (Char -> Bool) -> Parser Char
satisfy f = MkParser $ \input =>
    let c  = strHead input in
    let cs = strTail input in
    if (f c) then (cs, Parsed c) else (input, Failed ("Found " ++ (show c)))

char : Char -> Parser Char
char c = satisfy (== c)

string : String -> Parser String
string s = MkParser $ \input =>
    if (Prelude.Strings.length s) > (Prelude.Strings.length input)
       then (input, Failed "Not enough input")
       else let s' = strTake (Prelude.Strings.length s) input in
                if s == s'
                   then (strDrop (Prelude.Strings.length s) input, Parsed s')
                   else (input, Failed ("Does not match " ++ s))
    where strTake n str = let l = Prelude.Strings.unpack str in
                              Prelude.Strings.pack $ Prelude.List.take n l
          strDrop n str = let l = Prelude.Strings.unpack str in
                              Prelude.Strings.pack $ Prelude.List.drop n l

{- many : Parser a -> Parser (List (Result String a))
many (MkParser p) = MkParser $ \input => many' input p where 
    many' inp pf = case pf inp of
        (_, Failed e)    => (inp, Parsed Prelude.List.Nil)
        (rest, Parsed x) => let (rest', xs) = (many' rest pf)
            in x :: xs
    sequence (Parsed []) = []
    sequence ((Parsed x) :: xs) = Parsed $ x :: (sequence xs) -}
    
runParser : Parser a -> String -> Either String a
runParser (MkParser f) input = 
  case f input of
    (_, Failed e) => Left e
    (_, Parsed x) => Right x


data IdrisC = Code String | Directive String

instance Show IdrisC where
  show (Code s)      = "Code " ++ s
  show (Directive s) = "Directive " ++ s
 
{- idrDirective : Parser IdrisC
idrDirective = do
  _ <- char '#' -}
    


