module Parallax

class Monad m => MonadPlus (m : Type -> Type) where
    mzero : m a
    mplus : m a -> m a -> m a

data Result e a = Failed String e 
                | Parsed String a

instance Functor (Result e) where
  map _ (Failed r e) = Failed r e
  map f (Parsed r x) = Parsed r (f x)
    
data Parser a = MkParser (String -> Result String a)

instance Functor Parser where
    map g (MkParser f) = MkParser $ \input => map g $ f input

instance Applicative Parser where
    pure x = MkParser (\input => Parsed input x)

    (MkParser f) <$> x = MkParser $ \input => case f input of
        Failed r e  => Failed r e
        Parsed r f' => let (MkParser x') = x in map f' (x' r)
        
instance Alternative Parser where
    empty = MkParser $ \input => Failed input "ParserZero: no parsing to be performed."
    (MkParser a1) <|> a2 = MkParser $ \input =>
      case a1 input of
        Failed r e => let (MkParser a2') = a2 in a2' r
        result     => result

instance Monad Parser where
    (MkParser m) >>= f = MkParser $ \input =>
        case m input of
            Failed r e => Failed r e
            Parsed r x => 
                let (MkParser f') = f x
                in f' r

satisfy : (Char -> Bool) -> Parser Char
satisfy f = MkParser $ \input => case input of
    ""     => Failed "" "End of Input"
    input' => 
        let c  = strHead input' in
        let cs = strTail input' in
        if (f c) 
           then Parsed cs c 
           else Failed input' ("Found " ++ (show c))

many : Parser a -> Parser (List a)
many p = [| p :: lazy (many p) |] <|> pure []

many1 : Parser a -> Parser (List a)
many1 p = [| p :: many p |]
      
char : Char -> Parser Char
char c = satisfy (== c)

string : String -> Parser String
string s = MkParser $ \input =>
    if (Prelude.Strings.length s) > (Prelude.Strings.length input)
        then Failed input "Not enough input"
        else let s' = strTake (Prelude.Strings.length s) input in
            if s == s'
                then Parsed (strDrop (Prelude.Strings.length s) input) s'
                else Failed input ("Does not match " ++ s)
    where strTake n str = let l = Prelude.Strings.unpack str in -- inefficent 
                            Prelude.Strings.pack $ Prelude.List.take n l
          strDrop n str = let l = Prelude.Strings.unpack str in
                            Prelude.Strings.pack $ Prelude.List.drop n l

letter : Parser Char
letter = satisfy (\c => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))

runParser : Parser a -> String -> Either String a
runParser (MkParser f) input =
  case f input of
       Failed r e => Left e
       Parsed r x => Right x
