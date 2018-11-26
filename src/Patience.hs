{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Implements \"patience diff\" and the patience algorithm for the longest
--   increasing subsequence problem.
module Patience
  ( -- * Patience diff
    diff
  , Item(..), itemChar, itemValue
    -- * Longest increasing subsequence
  , longestIncreasing
  ) where

import           Data.Data       (Data)
import qualified Data.Foldable   as F
import qualified Data.IntMap     as IM
import           Data.List
import qualified Data.Map        as M
import qualified Data.Map.Strict as MS
import           Data.Ord
import           Data.Sequence   ( (<|), (|>), (><), ViewL(..), ViewR(..) )
import qualified Data.Sequence   as S
import           Data.Typeable   (Typeable)

-- If key xi is in the map, move it to xf while adjusting the value with f.
adjMove :: (a -> a) -> Int -> Int -> IM.IntMap a -> IM.IntMap a
adjMove f !xi !xf m = case IM.updateLookupWithKey (\_ _ -> Nothing) xi m of
  (Just v, mm) -> IM.insert xf (f v) mm
  (Nothing, _) -> m

-- A "card" is an integer value (with annotation) plus a "backpointer" to
-- a card in the previous pile, if any.
data Card a = Card !Int a (Maybe (Card a))

-- | Given: a list of distinct integers.  Picks a subset of the integers
--   in the same order, i.e. a subsequence, with the property that
--
--   * it is monotonically increasing, and
--
--   * it is at least as long as any other such subsequence.
--
-- This function uses patience sort:
-- <http://en.wikipedia.org/wiki/Patience_sorting>.
-- For implementation reasons, the actual list returned is the reverse of
-- the subsequence.
--
-- You can pair each integer with an arbitrary annotation, which will be
-- carried through the algorithm.
longestIncreasing :: [(Int,a)] -> [(Int,a)]
longestIncreasing = extract . F.foldl' ins IM.empty where
  -- Insert a card into the proper pile.
  -- type Pile  a = [Card a]
  -- type Piles a = IM.IntMap (Pile a)  -- keyed by smallest element
  ins m (x,a) =
    let (lt, gt) = IM.split x m
        prev = (head . fst) `fmap` IM.maxView lt
        new  = Card x a prev
    in case IM.minViewWithKey gt of
      Nothing        -> IM.insert x [new] m   -- new pile
      Just ((k,_),_) -> adjMove (new:) k x m  -- top of old pile
  -- Walk the backpointers, starting at the top card of the
  -- highest-keyed pile.
  extract (IM.maxView -> Just (c,_)) = walk $ head c
  extract _ = []
  walk (Card x a c) = (x,a) : maybe [] walk c

-- Elements whose second component appears exactly once.
unique :: (Ord t) => S.Seq (a,t) -> M.Map t a
unique = M.mapMaybe id . F.foldr ins M.empty where
  ins (a,x) = MS.insertWith (\_ _ -> Nothing) x (Just a)

-- Given two sequences of numbered "lines", returns a list of points
-- where unique lines match up.
solveLCS :: (Ord t) => S.Seq (Int,t) -> S.Seq (Int,t) -> [(Int,Int)]
solveLCS ma mb =
  let xs = M.elems $ M.intersectionWith (,) (unique ma) (unique mb)
  in  longestIncreasing $ sortBy (comparing snd) xs

-- Type for decomposing a diff problem.  We either have two
-- lines that match, or a recursive subproblem.
data Piece a
  = Match a a
  | Diff (S.Seq a) (S.Seq a)
  deriving (Show)

-- Subdivides a diff problem according to the indices of matching lines.
chop :: S.Seq t -> S.Seq t -> [(Int,Int)] -> [Piece t]
chop xs ys []
  | S.null xs && S.null ys = []
  | otherwise = [Diff xs ys]
chop xs ys (!(!nx,!ny):ns) =
  let (xsr, S.viewl -> (x :< xse)) = S.splitAt nx xs
      (ysr, S.viewl -> (y :< yse)) = S.splitAt ny ys
  in  Diff xse yse : Match x y : chop xsr ysr ns

-- Zip a list with a Seq.
zipLS :: [a] -> S.Seq b -> S.Seq (a, b)
#if MIN_VERSION_containers(0,3,0)
zipLS = S.zip . S.fromList
#else
zipLS xs = S.fromList . zip xs . F.toList
#endif

-- Number the elements of a Seq.
number :: S.Seq t -> S.Seq (Int,t)
number xs = zipLS [0..S.length xs - 1] xs

-- | An element of a computed difference.
data Item t
  = Old  t    -- ^ Value taken from the \"old\" list, i.e. left argument to 'diff'
  | New  t    -- ^ Value taken from the \"new\" list, i.e. right argument to 'diff'
  | Both t t  -- ^ Value taken from both lists.  Both values are provided, in case
              --   your type has a non-structural definition of equality.
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Functor Item where
  fmap f (Old  x  ) = Old  (f x)
  fmap f (New  x  ) = New  (f x)
  fmap f (Both x y) = Both (f x) (f y)

-- | The difference between two lists, according to the
-- \"patience diff\" algorithm.
diff :: (Ord t) => [t] -> [t] -> [Item t]
diff xsl ysl = F.toList $ go (S.fromList xsl) (S.fromList ysl) where
  -- Handle common elements at the beginning / end.
  go (S.viewl -> (x :< xs)) (S.viewl -> (y :< ys))
    | x == y = Both x y <| go xs ys
  go (S.viewr -> (xs :> x)) (S.viewr -> (ys :> y))
    | x == y = go xs ys |> Both x y
  -- Find an increasing sequence of matching unique lines, then
  -- subdivide at those points and recurse.
  go xs ys = case chop xs ys $ solveLCS (number xs) (number ys) of
    -- If we fail to subdivide, just record the chunk as is.
    [Diff _ _] -> fmap Old xs >< fmap New ys
    ps -> recur ps

  -- Apply the algorithm recursively to a decomposed problem.
  -- The decomposition list is in reversed order.
  recur [] = S.empty
  recur (Match x y  : ps) = recur ps |> Both x y
  recur (Diff xs ys : ps) = recur ps >< go xs ys

-- | The character @\'-\'@ or @\'+\'@ or @\' \'@ for 'Old' or 'New' or 'Both' respectively.
itemChar :: Item t -> Char
itemChar (Old  _  ) = '-'
itemChar (New  _  ) = '+'
itemChar (Both _ _) = ' '

-- | The value from an 'Item'.  For 'Both', returns the \"old\" value.
itemValue :: Item t -> t
itemValue (Old  x  ) = x
itemValue (New  x  ) = x
itemValue (Both x _) = x
