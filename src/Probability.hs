-- |
--
-- Module      :  Probability
-- Description :
-- License     :  MIT
-- Stability   :  dev
--
--

module Probability (

  Event(Ev, Universal, Null)
, P

, CreateP ((~~))
, (~|)

, union
, (&)

, intersection
, (#)

, complement
, neg

) where

import Data.Set (Set, (\\))
import Data.List(intercalate)
import qualified Data.Set as Set

-- | Event container
data Event ev = Ev ev                       -- ^ An atomic event.
              | Union      (Set (Event ev)) -- ^ events union.
              | Intersect  (Set (Event ev)) -- ^ events intersection.
              | Complement (Event ev)       -- ^ event negation.
              | Universal                   -- ^ event universal set, result of @a & 'a@
              | Null                        -- ^ empty, result of @a # 'a@
              deriving (Eq, Ord)


instance (Show ev) =>
    Show (Event ev) where
        show (Ev e)         = show e
        show (Union es)     = "(" ++ intercalate "&" (map show $ Set.toList es) ++ ")"
        show (Intersect es) = "(" ++ intercalate "#" (map show $ Set.toList es) ++ ")"
        show (Complement e) = "'" ++ show e
        show Universal      = "Universe"
        show Null           = "Null"


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

union x@(Ev _)         y@(Intersect ys)  = y `union` x
union x@(Intersect xs) y@(Ev _) | y `Set.member` xs = y
                                | otherwise         = mkUnion $ Set.fromList [x, y]

union x@(Union xs)     y@(Intersect ys) = if any (`Set.member` xs) $ Set.elems ys
                                            then x
                                            else mkUnion $ y `Set.insert` xs
union x@(Intersect _)  y@(Union _)       = y `union` x
union x@(Intersect xs) y@(Intersect ys)  = mkIntersect $ Set.fromList l
                                         where common = xs `Set.intersection` ys
                                               diff   = (xs \\ ys) `Set.union` (ys \\ xs)
                                               l = if not . Set.null $ common
                                                    then [mkIntersect common, mkUnion diff]
                                                    else [x, y]

union (Complement x) (Complement y) = Complement $ x `intersection` y
union x (Complement y) | x == y     = Universal
union (Complement x) y | x == y     = Universal

union Universal _ = Universal
union _ Universal = Universal

union Null x = x
union x Null = x

union x y = Union $ Set.fromList [x, y]

-- | alias for 'union'
x & y = x `union` y

-- | Two events intersection
intersection :: (Ord ev) => Event ev -> Event ev -> Event ev

intersection x@(Ev _)           (Intersect ys)  = mkIntersect $ Set.insert x ys
intersection x@(Intersect _)  y@(Ev _)          = y `intersection` x
intersection   (Intersect xs)   (Intersect ys)  = mkIntersect $ xs `Set.union` ys

intersection x@(Ev _)         y@(Union ys) | x `Set.member` ys = x
                                           | otherwise         = mkIntersect $ Set.fromList [x,y]
intersection x@(Union _)      y@(Ev _)          = y `intersection` x

intersection x@(Intersect xs) y@(Union ys) | any (`Set.member` xs) $ Set.elems ys = x
                                           | otherwise = mkIntersect $ Set.fromList [x,y]
intersection x@(Union _)      y@(Intersect _)   = y `intersection` x
intersection x@(Union xs)     y@(Union ys)      = mkUnion $ Set.fromList l
                                           where common = xs `Set.intersection` ys
                                                 diff   = (xs \\ ys) `Set.union` (ys \\ xs)
                                                 l = if not . Set.null $ common
                                                    then [mkUnion common, mkIntersect diff]
                                                    else [x, y]

intersection (Complement x) (Complement y) = Complement $ x `union` y
intersection x (Complement y) | x == y     = Null
intersection (Complement x) y | x == y     = Null

intersection Universal x = x
intersection x Universal = x

intersection Null _ = Null
intersection _ Null = Null

intersection x y = mkIntersect $ Set.fromList [x,y]

-- | alias for 'intersection'
x # y = x `intersection` y


-- | event complement
complement :: Event ev -> Event ev

complement (Complement e) = e
complement Universal = Null
complement Null = Universal
complement e = Complement e

-- | alias for 'complement'
neg = complement





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


