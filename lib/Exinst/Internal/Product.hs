{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Exinst.Internal.Product
 ( P1(P1)
 , P2(P2)
 , P3(P3)
 , P4(P4)
 ) where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Bin
import GHC.Generics (Generic)

#ifdef HAS_aeson
import Data.Aeson (FromJSON, ToJSON)
#endif

#ifdef HAS_bytes
import qualified Data.Bytes.Serial as By
#endif

#ifdef HAS_cereal
import qualified Data.Serialize as Cer
#endif


#ifdef HAS_hashable
import Data.Hashable (Hashable)
#endif

#ifdef HAS_quickcheck
import qualified Test.QuickCheck as QC
#endif

#ifdef HAS_serialise
import qualified Codec.Serialise as Cborg
#endif

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

--------------------------------------------------------------------------------
#ifdef HAS_hashable
instance (Hashable (l a1), Hashable (r a1)) => Hashable (P1 l r a1)
instance (Hashable (l a2 a1), Hashable (r a2 a1)) => Hashable (P2 l r a2 a1)
instance (Hashable (l a3 a2 a1), Hashable (r a3 a2 a1)) => Hashable (P3 l r a3 a2 a1)
instance (Hashable (l a4 a3 a2 a1), Hashable (r a4 a3 a2 a1)) => Hashable (P4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
instance (NFData (l a1), NFData (r a1)) => NFData (P1 l r a1)
instance (NFData (l a2 a1), NFData (r a2 a1)) => NFData (P2 l r a2 a1)
instance (NFData (l a3 a2 a1), NFData (r a3 a2 a1)) => NFData (P3 l r a3 a2 a1)
instance (NFData (l a4 a3 a2 a1), NFData (r a4 a3 a2 a1)) => NFData (P4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------
#ifdef HAS_aeson
instance (FromJSON (l a1), FromJSON (r a1)) => FromJSON (P1 l r a1)
instance (FromJSON (l a2 a1), FromJSON (r a2 a1)) => FromJSON (P2 l r a2 a1)
instance (FromJSON (l a3 a2 a1), FromJSON (r a3 a2 a1)) => FromJSON (P3 l r a3 a2 a1)
instance (FromJSON (l a4 a3 a2 a1), FromJSON (r a4 a3 a2 a1)) => FromJSON (P4 l r a4 a3 a2 a1)

instance (ToJSON (l a1), ToJSON (r a1)) => ToJSON (P1 l r a1)
instance (ToJSON (l a2 a1), ToJSON (r a2 a1)) => ToJSON (P2 l r a2 a1)
instance (ToJSON (l a3 a2 a1), ToJSON (r a3 a2 a1)) => ToJSON (P3 l r a3 a2 a1)
instance (ToJSON (l a4 a3 a2 a1), ToJSON (r a4 a3 a2 a1)) => ToJSON (P4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef HAS_bytes
instance (By.Serial (l a1), By.Serial (r a1)) => By.Serial (P1 l r a1)
instance (By.Serial (l a2 a1), By.Serial (r a2 a1)) => By.Serial (P2 l r a2 a1)
instance (By.Serial (l a3 a2 a1), By.Serial (r a3 a2 a1)) => By.Serial (P3 l r a3 a2 a1)
instance (By.Serial (l a4 a3 a2 a1), By.Serial (r a4 a3 a2 a1)) => By.Serial (P4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef HAS_cereal
instance (Cer.Serialize (l a1), Cer.Serialize (r a1)) => Cer.Serialize (P1 l r a1)
instance (Cer.Serialize (l a2 a1), Cer.Serialize (r a2 a1)) => Cer.Serialize (P2 l r a2 a1)
instance (Cer.Serialize (l a3 a2 a1), Cer.Serialize (r a3 a2 a1)) => Cer.Serialize (P3 l r a3 a2 a1)
instance (Cer.Serialize (l a4 a3 a2 a1), Cer.Serialize (r a4 a3 a2 a1)) => Cer.Serialize (P4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
instance (Bin.Binary (l a1), Bin.Binary (r a1)) => Bin.Binary (P1 l r a1)
instance (Bin.Binary (l a2 a1), Bin.Binary (r a2 a1)) => Bin.Binary (P2 l r a2 a1)
instance (Bin.Binary (l a3 a2 a1), Bin.Binary (r a3 a2 a1)) => Bin.Binary (P3 l r a3 a2 a1)
instance (Bin.Binary (l a4 a3 a2 a1), Bin.Binary (r a4 a3 a2 a1)) => Bin.Binary (P4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------
#ifdef HAS_quickcheck
instance (QC.Arbitrary (l a1), QC.Arbitrary (r a1)) => QC.Arbitrary (P1 l r a1) where
  arbitrary = P1 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P1 x y) = P1 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (l a2 a1), QC.Arbitrary (r a2 a1)) => QC.Arbitrary (P2 l r a2 a1) where
  arbitrary = P2 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P2 x y) = P2 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (l a3 a2 a1), QC.Arbitrary (r a3 a2 a1)) => QC.Arbitrary (P3 l r a3 a2 a1) where
  arbitrary = P3 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P3 x y) = P3 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (l a4 a3 a2 a1), QC.Arbitrary (r a4 a3 a2 a1)) => QC.Arbitrary (P4 l r a4 a3 a2 a1) where
  arbitrary = P4 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P4 x y) = P4 <$> QC.shrink x <*> QC.shrink y
#endif

--------------------------------------------------------------------------------
#ifdef HAS_serialise
instance (Cborg.Serialise (l a1), Cborg.Serialise (r a1)) => Cborg.Serialise (P1 l r a1)
instance (Cborg.Serialise (l a2 a1), Cborg.Serialise (r a2 a1)) => Cborg.Serialise (P2 l r a2 a1)
instance (Cborg.Serialise (l a3 a2 a1), Cborg.Serialise (r a3 a2 a1)) => Cborg.Serialise (P3 l r a3 a2 a1)
instance (Cborg.Serialise (l a4 a3 a2 a1), Cborg.Serialise (r a4 a3 a2 a1)) => Cborg.Serialise (P4 l r a4 a3 a2 a1)
#endif
