{-# language DeriveGeneric       #-}
{-# language DeriveFoldable      #-}
{-# language DeriveFunctor       #-}
{-# language DeriveTraversable   #-}

module Patience.Delta
  ( Delta(..)
  )
  where

import GHC.Generics (Generic, Generic1)

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
  = Delta !a !a
  | Same !a
  | Old  !a
  | New  !a
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)
