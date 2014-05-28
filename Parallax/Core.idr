module Parallax.Core

import Parallax.Buffer
import Parallax.Types

Parser : Type -> Type
Parser a = Parallax.Types.Parser String Buffer a

Result : Type -> Type
Result a = IResult String a

Failure : Type -> Type
Failure r = Parallax.Types.Failure String Buffer r

Success : Type -> Type -> Type
Success a r = Parallax.Types.Success String Buffer a r

-- note be careful about mixing constructor/type name
failK : Failure a
failK t (MkPos pos) more stack msg = Fail (Buffer.unsafeDrop pos t) stack msg

successK : Success a a
successK t (MkPos pos) more a = Done (Buffer.unsafeDrop pos t) a

parse : Parser a -> String -> Result a
parse p input = Parallax.Types.runParser p (buffer input) (MkPos 0) Incomplete failK successK
