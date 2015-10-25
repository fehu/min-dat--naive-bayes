-- |
--
-- Module      :  Event.Probability
-- Description :
-- License     :  MIT
-- Stability   :  dev
--
--

module Event.Probability (

  P

, CreateP ((~~))
, (~|)


----------------------------

, Event(Ev, Universal, Null)

, union
, (&)

, intersection
, (#)

, complement
, neg

) where

import Event

-----------------------------------------------------------------------------

-- | Probability
data P ev = P  (Event ev) Float             -- ^ probability of an event
          | PC (Event ev) (Event ev) Float  -- ^ conditional probability


instance Show ev =>
    Show (P ev) where show (P ev p)    = "P(" ++ show ev ++ ")=" ++ show p
                      show (PC ev c p) = "P(" ++ show ev ++ "|" ++ show c  ++ ")=" ++ show p


class CreateP ev from where (~~) :: from ev -> Float -> P ev -- ^ probability constructor

instance CreateP ev Event   where ev        ~~ p = P ev p
instance CreateP ev PC'     where (PC' e c) ~~ p = PC e c p

assertP :: Float -> Float
assertP p | p >= 0 && p <= 1 = p
          | otherwise        = error $ "probability must be in range [0,1], got " ++ show p

data PC' ev = PC' (Event ev) (Event ev)



(~|) :: Event ev -> Event ev -> PC' ev
e ~| cond = PC' e cond


