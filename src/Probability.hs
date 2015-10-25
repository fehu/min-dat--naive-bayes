-- |
--
-- Module      :  Probability
-- Description :
-- License     :  MIT
-- Stability   :  dev
--
--

module Probability (

  Event(Ev)
, P

, CreateP ((~~))
, (~|)

, union
, (~&)

, intersection
, (~/)

) where

import Data.Set (Set, (\\))
import Data.List(intercalate)
import qualified Data.Set as Set

-- | Event container
data Event ev = Ev ev                       -- ^ An atomic event.
              | Union      (Set (Event ev)) -- ^ events union.
              | Intersect  (Set (Event ev)) -- ^ events intersection.
              | Complement (Event ev)       -- ^ event negation.
              deriving (Eq, Ord)

instance Show ev =>
    Show (Event ev) where
        show (Ev e)         = show e
        show (Union es)     = "(" ++ intercalate "&" (map show $ Set.toList es) ++ ")"
        show (Intersect es) = "(" ++ intercalate "#" (map show $ Set.toList es) ++ ")"
        show (Complement e) = "'" ++ show e


mkEvent f set | Set.null set      = error "empty union"
              | Set.size set == 1 = Set.findMax set
              | otherwise         = f set

mkUnion = mkEvent Union
mkIntersect = mkEvent Intersect

-- | Two events union
union :: (Ord ev) => Event ev -> Event ev -> Event ev

--union x@(Ev _)    y@(Ev _)      = Union $ Set.fromList [x, y]
union x@(Ev _)      (Union ys)  = mkUnion $ Set.insert x ys
union x@(Union _) y@(Ev _)      = y `union` x
union (Union xs)    (Union ys)  = mkUnion $ xs `Set.union` ys

union x@(Ev _)          y@(Intersect ys)  = y `union` x
union x@(Intersect xs)  y@(Ev _) | y `Set.member` xs = y
                                 | otherwise         = mkUnion $ Set.fromList [x, y]

union x@(Union xs) y@(Intersect ys) = if any (`Set.member` xs) $ Set.elems ys
                                        then x
                                        else mkUnion $ Set.fromList [x,y]
                                    --Union . Set.insert (Set.filter (`Set.notMember` xs)) ys
union x@(Intersect _) y@(Union _) = y `union` x
union (Intersect xs) (Intersect ys) = Intersect $ Set.fromList [mkIntersect common, mkUnion diff]
                                    where common = xs `Set.intersection` ys
                                          diff   = (xs \\ ys) `Set.union` (ys \\ xs)

union x y = Union $ Set.fromList [x, y]

-- | alias for 'union'
x ~& y = x `union` y


-- | Two events intersection
intersection :: (Ord ev) => Event ev -> Event ev -> Event ev

intersection x@(Ev _)          (Intersect ys)  = mkIntersect $ Set.insert x ys
intersection x@(Intersect _) y@(Ev _)          = y `intersection` x
intersection   (Intersect xs)  (Intersect ys)  = mkIntersect $ xs `Set.union` ys

intersection x@(Intersect _) y@(Union _)       = mkIntersect $ Set.fromList [x,y]
intersection x@(Union _)     y@(Intersect _)   = y `intersection` x
intersection x y = Intersect $ Set.fromList [x,y]

-- | alias for intersection
x ~/ y = x `intersection` y

-- | Events union
--unions :: (Ord ev) => [Event ev] -> Event ev
--unions []  = error "no events to union"
--unions [e] = e
--unions es  = Union $ Set.fromList es


--intersections :: (Ord ev) => [Event ev] -> Event ev
--intersections es = Intersect $ Set.fromList es



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


