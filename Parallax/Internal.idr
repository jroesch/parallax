module Parallax.Internal

import Parallax.Types

compareResults : (Eq i, Eq r) => IResult i r -> IResult i r -> Maybe Bool
compareResults (Fail t0 ctxs0 msg0) (Fail t1 ctxs1 msg1) =
  Just (t0 == t1 && ctxs0 == ctxs1 && msg0 == msg1)
compareResults (Done t0 r0) (Done t1 r1) =
  Just (t0 == t1 && r0 == r1)
compareResults (Partial _) (Partial _) = Nothing
compareResults _ _ = Just False

prompt : State t -> Pos -> More
       -> (State t -> Pos -> More -> IResult t r)
       -> (State t -> Pos -> More -> IResult t r)
       -> IResult t r
prompt = ?prompt
