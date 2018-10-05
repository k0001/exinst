{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Exinst.Internal.Sum
 ( S1(S1L,S1R)
 , S2(S2L,S2R)
 , S3(S3L,S3R)
 , S4(S4L,S4R)
 ) where

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Sums

-- | Like 'Data.Functor.Sum.Sum' from "Data.Functor.Sum", but
-- only intended to be used with kinds other than 'Type'.
--
-- This type is particularly useful when used in combination with 'Exinst.Some1'
-- as @'Exinst.Some1' ('S1' l r)@, so as to ensure that @l@ and @r@ are indexed
-- by the same type. Moreover, 'S1' already supports many common instances from
-- @base@, @hashable@, @deepseq@, @aeson@, @bytes@, @cereal@, @binary@, and
-- @quickcheck@ out of the box, so you can benefit from them as well.
data S1 l r (a1 :: k1)
  = S1L (l a1) | S1R (r a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'S1', but for @l@ and @r@ taking two type indexes.
data S2 l r (a2 :: k2) (a1 :: k1)
  = S2L (l a2 a1) | S2R (r a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'S1', but for @l@ and @r@ taking three type indexes.
data S3 l r (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = S3L (l a3 a2 a1) | S3R (r a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'S1', but for @l@ and @r@ taking four type indexes.
data S4 l r (a4 :: k4) (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = S4L (l a4 a3 a2 a1) | S4R (r a4 a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

