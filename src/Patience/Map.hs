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
    DeltaUnit(..)
  , Delta(..)
  , M(..)

    -- * Diffing 
  , diff

    -- * Case analysis on 'Delta'
  , getSame
  , getOld
  , getNew
  , getDelta
  , getOriginal
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
  , toOriginal
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
import           Data.Foldable         (Foldable)
import           Data.Function         ((.))
import           Data.Functor          (Functor(fmap))
import           Data.Maybe            (Maybe(Just,Nothing))
import           Data.Ord              (Ord)
import           Data.Traversable      (Traversable)
import           GHC.Generics          (Generic, Generic1)
import           GHC.Show              (Show)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as DMS
import qualified Data.Map.Merge.Strict as Merge

-- | Encodes a diff between two 'as'.
data DeltaUnit a = DeltaUnit
  { old :: !a
  , new :: !a
  }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

-- | The result of a diff of an entry within two 'Map's.
--
--   In two 'Map's m1 and m2, when performing a diff, this type encodes the following situations:
--
--   Same key, different values: Stores the two values in the Delta constructor.
--
--   Same key, same values: Stores the value in the Same constructor.
--
--   Key exists in m1 but not m2: Stores the value in the Old constructor.
--
--   Key exists in m2 but not m1: Stores the value in the New constructor.
--
--   This behaviour ensures that we don't lose any information, meaning
--   we can reconstruct either of the original 'Map' 'k' 'a' from a 'Map' 'k' ('Delta' 'a').
--   (Note that this slightly differs from `Patience.diff`, which does not
--   care about the possibility of reconstruction).
data Delta a
  = Delta !(DeltaUnit a)
  | Same !a
  | Old  !a
  | New  !a
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

-- | M1 = First 'Map', M2 = Second 'Map'.
--   Used as an argument for functions that care about which 'Map' to reconstruct.
data M = M1 | M2

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
    (Merge.zipWithMatched (\_ v1 v2 -> if v1 == v2 then Same v1 else Delta (DeltaUnit v1 v2)))
    m1
    m2
{-# INLINABLE diff #-}

-- | Is the 'Delta' an encoding of same values?
isSame :: Eq a => Delta a -> Bool
isSame (Same                _) = True
isSame (Delta (DeltaUnit x y)) =
  if x == y
  then True
  else False
isSame                      _  = False
{-# INLINABLE isSame #-}

-- | Is the 'Delta' an encoding of old values?
isOld :: Delta a -> Bool
isOld (Old                _ ) = True
isOld (Delta (DeltaUnit _ _)) = True
isOld                     _   = False
{-# INLINE isOld #-}

-- | Is the 'Delta' an encoding of new values?
isNew :: Delta a -> Bool
isNew (New                _ ) = True
isNew (Delta (DeltaUnit _ _)) = True
isNew                     _   = False
{-# INLINE isNew #-}

-- | Is the 'Delta' an encoding of changed values?
isDelta :: Delta a -> Bool
isDelta (Delta _) = True
isDelta        _  = False
{-# INLINE isDelta #-}

-- | Potentially get the 'Same' value out of a 'Delta'.
getSame :: Eq a => Delta a -> Maybe a
getSame (Same a) = Just a
getSame (Delta (DeltaUnit x y)) =
  if x == y
  then Just x
  else Nothing
getSame _        = Nothing
{-# INLINABLE getSame #-}

-- | Potentially get the 'Old' value out of a 'Delta'.
getOld :: Delta a -> Maybe a
getOld (Delta (DeltaUnit a _)) = Just a
getOld (Old a)                 = Just a
getOld _                       = Nothing
{-# INLINE getOld #-}

-- | Potentially get the 'New' value out of a 'Delta'.
getNew :: Delta a -> Maybe a
getNew (Delta (DeltaUnit _ a)) = Just a
getNew (New a)                 = Just a
getNew _                       = Nothing
{-# INLINE getNew #-}

-- | Potentially get the 'DeltaUnit' value out of a 'Delta'.
getDelta :: Delta a -> Maybe (DeltaUnit a)
getDelta (Delta d) = Just d
getDelta _         = Nothing
{-# INLINE getDelta #-}  

-- | Potentially get the original value out of the 'Delta'.
getOriginal :: M -> Delta a -> Maybe a
getOriginal M1 (Delta (DeltaUnit x _)) = Just x
getOriginal M2 (Delta (DeltaUnit _ y)) = Just y
getOriginal _  (Same x)                = Just x
getOriginal M1 (Old x)                 = Just x
getOriginal _  (Old _)                 = Nothing
getOriginal M2 (New x)                 = Just x
getOriginal _  (New _)                 = Nothing
{-# INLINE getOriginal #-}

-- | Get the original values out of the 'Delta'.
getOriginals :: Delta a -> (Maybe a, Maybe a)
getOriginals (Delta (DeltaUnit x y)) = (Just x, Just y)
getOriginals (Same x) = (Just x, Just x)
getOriginals (Old x) = (Just x, Nothing)
getOriginals (New x) = (Nothing, Just x)
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
        -> Map k (DeltaUnit a)
toDelta = DMS.mapMaybe getDelta
{-# INLINE toDelta #-}

-- | Construct either the old 'Map' or new 'Map' from a diff
toOriginal :: M
           -> Map k (Delta a)
           -> Map k a
toOriginal m = DMS.mapMaybe (getOriginal m)
{-# INLINE toOriginal #-}

-- | Reconstruct both original 'Map's.
toOriginals :: Map k (Delta a)
            -> (Map k a, Map k a)
toOriginals m = (DMS.mapMaybe (getOriginal M1) m, DMS.mapMaybe (getOriginal M2) m)

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
    go (Delta (DeltaUnit x y)) = Delta (DeltaUnit (f x) y)
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
    go (Delta (DeltaUnit x y)) = Delta (DeltaUnit x (f y))
    go x = x
{-# INLINE mapNew' #-}

