{-# LANGUAGE UnicodeSyntax, Rank2Types #-}
-- | Collection of functions of more general purpose.
module SoccerFun.Prelude     where

import Data.Maybe
import Control.Monad.Identity


fst3 (x,y,z) = x
snd3 (x,y,z) = y
thd3 (x,y,z) = z

avg ∷ Fractional a ⇒ [a] → a
avg xs = sum xs / fromIntegral (length xs)

zero ∷ Num a ⇒ a
zero = fromIntegral 0

one ∷ Num a ⇒ a
one = fromIntegral 1

iterateStn ∷ Int → (s → (a,s)) → s → ([a],s)
iterateStn 0 _ s    = ([],s)
iterateStn n f s = runIdentity $ do
    (a, s)        ← return $ f s
    (as,s)        ← return $ iterateStn (n-1) f s
    return $ ((a:as),s)

singleOutElems ∷ [a] → [(a,[a])]
singleOutElems as = singleOut [] as where
    singleOut ∷ [a] → [a] → [(a,[a])]
    singleOut _ [] = []
    singleOut prefix (a:as)= ((a,prefix++as) : singleOut (prefix++[a]) as)

-- | spanfilter cond xs = (filter cond xs, filter (not . cond) xs)
spanfilter ∷ (a → Bool) → [a] → ([a],[a])
spanfilter cond []
    = ([],[])
spanfilter cond (x:xs)
    | cond x    = ((x:yes),no)
    | otherwise    = (yes,(x:no))
    where
    (yes,no)    = spanfilter cond xs

spanfilterSt ∷ (a → s → (Bool,s)) → [a] → s → (([a],[a]),s)
spanfilterSt cond [] s
    = (([],[]),s)
spanfilterSt cond (x:xs) s = runIdentity $ do
    (ok,s)        ← return $ cond x s
    ((yes,no),s)    ← return $ spanfilterSt cond xs s
    return $ if ok    then (((x:yes),no),s) else ((yes,(x:no)),s)

{-| break cond (A ++ B ++ C) = (A,B,C)
        where for each x in A: not cond x     /\
          for each x in B:     cond x     /\
          if C=(x:_):      not cond x
-}
break' ∷ (a → Bool) → [a] → ([a],[a],[a])
break' c xs = let
        (no,yes)     = span (not . c) xs
        (yes',no') = span c yes
    in (no,yes',no')

{-| break1 cond (A ++ [B] ++ C) = (A,B,C)
        where for each x in A: not cond x     /\
                               cond B     /\
          if C=(x:_):      not cond x
-}
break1 ∷ (a → Bool) → [a] → ([a],a,[a])
break1 c xs
    = case break' c xs of
        (a,[b],c)    → (a,b,c)
        (a,b,c)        → error ("break1: [B] is of length: " ++ show (length b) ++ "\n")

-- | unbreak (a,b,c) = a ++ b ++ c
unbreak ∷ ([a],[a],[a]) → [a]
unbreak (a,b,c) = a ++ b ++ c


-- | unbreak1 (a,b,c) = a ++ [b] ++ c
unbreak1 ∷ ([a],a,[a]) → [a]
unbreak1 (a,b,c) = a ++ [b] ++ c

{- [a1..x..aN] x = i
        where
        aJ /= x for all j<i
        aJ == x for j==i
-}
--(??) infixl 9 ∷ [a] → a → Int | == a
--(??) ys x = search ((==) x) ys 0
--
--(???) infixl 9 ∷ [a] → (a → Bool) → Int
--(???) ys c = search c ys 0

type AssocList k v = [(k,v)]

boundedBy ∷ Ord a ⇒ a → (a,a) → a
boundedBy x (low,up)
    | low > x    = low
    | x > up    = up
    | otherwise    = x

{-| isbetween x low up
        returns True iff low <= x <= up
-}
isbetween ∷ Ord a ⇒ a → a → a → Bool
isbetween x low up
    = low <= x && x <= up

-- | minmax (a,b) = (a,b) if a<=b; (b,a) otherwise
minmax ∷ Ord a ⇒ (a,a) → (a,a)
minmax (a,b)
    | a<=b        = (a,b)
    | otherwise    = (b,a)

-- | perhaps p Nothing = False, and perhaps p (Just a) = p a
perhaps ∷ (a → Bool) → (Maybe a) → Bool
perhaps p = maybe False p

removeMember ∷ (Eq a) ⇒ a → [a] → [a]
removeMember x = filter (/= x)

removeMembers ∷ (Eq a) ⇒ [a] → [a] → [a]
removeMembers xs ys = foldr removeMember xs ys
