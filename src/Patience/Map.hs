{-# language BangPatterns        #-}
{-# language DeriveGeneric       #-}
{-# language DeriveFoldable      #-}
{-# language DeriveFunctor       #-}
{-# language DeriveTraversable   #-}
{-# language NoImplicitPrelude   #-}
{-# language ScopedTypeVariables #-}

-- | This module provides a lossless way to do
--   diffing between two 'Map's, and ways to
--   manipulate the diffs.
module Patience.Map
  ( -- * Types
    Delta(..)

    -- * Diffing
  , diff

    -- * Case analysis on 'Delta'
  , getSame
  , getOld
  , getNew
  , getDelta
  , getOriginals

  , isSame
  , isOld
  , isNew
  , isDelta

    -- * Construction of special maps from a diff
  , toSame
  , toOld
  , toNew
  , toDelta
  , toOriginals

    -- * Mapping
  , mapSame
  , mapOld
  , mapNew

  , mapSame'
  , mapOld'
  , mapNew'
  ) where

import           Data.Bool             (Bool(True, False))
import           Data.Eq               (Eq((==)))
import           Data.Function         ((.))
import           Data.Functor          (Functor(fmap))
import           Data.Maybe            (Maybe(Just,Nothing))
import           Data.Ord              (Ord)
import           Data.Tuple            (fst,snd)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as DMS
import qualified Data.Map.Merge.Strict as Merge

import           Patience.Delta (Delta(..))

-- | Takes two 'Map's and returns a 'Map' from the same key type to 'Delta' 'a',
--   where 'Delta' 'a' encodes differences between entries.
diff :: (Eq a, Ord k)
     => Map k a -- ^ first, /old/ 'Map'
     -> Map k a -- ^ second, /new/ 'Map'
     -> Map k (Delta a) -- ^ 'Map' encoding the diff
diff !m1 !m2 =
  Merge.merge
    (Merge.mapMissing (\_ x -> Old x)) -- preserve keys found in m1 but not m2
    (Merge.mapMissing (\_ x -> New x)) -- preserve keys found in m2 but not m1
    (Merge.zipWithMatched (\_ v1 v2 -> if v1 == v2 then Same v1 else Delta v1 v2))
    m1
    m2
{-# INLINABLE diff #-}

-- | Is the 'Delta' an encoding of same values?
isSame :: Eq a => Delta a -> Bool
isSame (Same    _) = True
isSame (Delta x y) = x == y
isSame          _  = False
{-# INLINABLE isSame #-}

-- | Is the 'Delta' an encoding of old values?
isOld :: Delta a -> Bool
isOld (Old     _) = True
isOld (Delta _ _) = True
isOld           _ = False
{-# INLINE isOld #-}

-- | Is the 'Delta' an encoding of new values?
isNew :: Delta a -> Bool
isNew (New     _) = True
isNew (Delta _ _) = True
isNew           _ = False
{-# INLINE isNew #-}

-- | Is the 'Delta' an encoding of changed values?
isDelta :: Delta a -> Bool
isDelta (Delta _ _) = True
isDelta           _ = False
{-# INLINE isDelta #-}

-- | Potentially get the 'Same' value out of a 'Delta'.
getSame :: Eq a => Delta a -> Maybe a
getSame (Same a)    = Just a
getSame (Delta x y) = if x == y then Just x else Nothing
getSame           _ = Nothing
{-# INLINABLE getSame #-}

-- | Potentially get the 'Old' value out of a 'Delta'.
getOld :: Delta a -> Maybe a
getOld (Delta a _) = Just a
getOld (Old a)     = Just a
getOld           _ = Nothing
{-# INLINE getOld #-}

-- | Potentially get the 'New' value out of a 'Delta'.
getNew :: Delta a -> Maybe a
getNew (Delta _ a) = Just a
getNew (New a)     = Just a
getNew           _ = Nothing
{-# INLINE getNew #-}

-- | Potentially get the 'Changed' value out of a 'Delta'.
getDelta :: Delta a -> Maybe (a,a)
getDelta (Delta d1 d2) = Just (d1,d2)
getDelta             _ = Nothing
{-# INLINE getDelta #-}

-- | Get the original values out of the 'Delta'.
getOriginals :: Delta a -> (Maybe a, Maybe a)
getOriginals (Delta x y) = (Just x, Just y)
getOriginals (Same  x  ) = (Just x, Just x)
getOriginals (Old   x  ) = (Just x, Nothing)
getOriginals (New   x  ) = (Nothing, Just x)
{-# INLINE getOriginals #-}

-- | Retrieve the 'Same' values out of the diff map.
toSame :: Eq a => Map k (Delta a)
       -> Map k a
toSame = DMS.mapMaybe getSame
{-# INLINABLE toSame #-}

-- | Retrieve only the 'Old' values out of the diff map.
toOld :: Map k (Delta a)
      -> Map k a
toOld = DMS.mapMaybe getOld
{-# INLINE toOld #-}

-- | Retrieve only the 'New' values out of the diff map.
toNew :: Map k (Delta a)
      -> Map k a
toNew = DMS.mapMaybe getNew
{-# INLINE toNew #-}

-- | Retrieve only the 'DeltaUnit' values out of the diff map.
toDelta :: Map k (Delta a)
        -> Map k (a,a)
toDelta = DMS.mapMaybe getDelta
{-# INLINE toDelta #-}

-- | Reconstruct both original 'Map's.
toOriginals :: Map k (Delta a)
            -> (Map k a, Map k a)
toOriginals m = (DMS.mapMaybe (fst . getOriginals) m, DMS.mapMaybe (snd . getOriginals) m)

-- | Map over all 'Same' values, returning a map of just
--   the transformed values.
--   This can be more efficient than calling 'toSame' and
--   then Data.Map's 'DMS.map'.
mapSame :: Eq a
        => (a -> b)
        -> Map k (Delta a)
        -> Map k b
mapSame f = DMS.mapMaybe (fmap f . getSame)
{-# INLINABLE mapSame #-}

-- | Map over all 'Old' values, returning a map of just
--   the transformed values.
--   This can be more efficient than calling 'toOld' and
--   then Data.Map's 'DMS.map'.
mapOld :: (a -> b)
       -> Map k (Delta a)
       -> Map k b
mapOld f = DMS.mapMaybe (fmap f . getOld)
{-# INLINE mapOld #-}

-- | Map over all 'New' values, returning a map of just
--   the transformed values.
--   This can be more efficient than calling 'toNew' and
--   then Data.Map's 'DMS.map'.
mapNew :: (a -> b)
       -> Map k (Delta a)
       -> Map k b
mapNew f = DMS.mapMaybe (fmap f . getNew)
{-# INLINE mapNew #-}

-- | Map over all the 'Same' values, preserving the
--   remaining values in the map.
mapSame' :: Eq a
         => (a -> a)
         -> Map k (Delta a)
         -> Map k (Delta a)
mapSame' f = DMS.map (\x -> if isSame x then fmap f x else x)
{-# INLINABLE mapSame' #-}

-- | Map over all the 'Old' values, preserving the
--   remaining values in the map.
mapOld' :: (a -> a)
        -> Map k (Delta a)
        -> Map k (Delta a)
mapOld' f = DMS.map go
  where
    go (Old x) = Old (f x)
    go (Delta x y) = Delta (f x) y
    go x = x
{-# INLINE mapOld' #-}

-- | Map over all the 'New' values, preserving the
--   remaining values in the map.
mapNew' :: (a -> a)
        -> Map k (Delta a)
        -> Map k (Delta a)
mapNew' f = DMS.map go
  where
    go (New x) = New (f x)
    go (Delta x y) = Delta x (f y)
    go x = x
{-# INLINE mapNew' #-}
