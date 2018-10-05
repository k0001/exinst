{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Exinst.Internal.Product
 ( P1(P1)
 , P2(P2)
 , P3(P3)
 , P4(P4)
 ) where

import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Products

-- | Like 'Data.Functor.Product.Product' from "Data.Functor.Product", but
-- only intended to be used with kinds other than 'Type'.
--
-- This type is particularly useful when used in combination with 'Exinst.Some1'
-- as @'Exinst.Some1' ('P1' l r)@, so as to ensure that @l@ and @r@ are indexed
-- by the same type. Moreover, 'P1' already supports many common instances from
-- @base@, @hashable@, @deepseq@, @aeson@, @bytes@, @cereal@, @binary@, and
-- @quickcheck@ out of the box, so you can benefit from them as well.
data P1 l r (a1 :: k1)
  = P1 (l a1) (r a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @l@ and @r@ taking two type indexes.
data P2 l r (a2 :: k2) (a1 :: k1)
  = P2 (l a2 a1) (r a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @l@ and @r@ taking three type indexes.
data P3 l r (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = P3 (l a3 a2 a1) (r a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @l@ and @r@ taking four type indexes.
data P4 l r (a4 :: k4) (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = P4 (l a4 a3 a2 a1) (r a4 a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)
