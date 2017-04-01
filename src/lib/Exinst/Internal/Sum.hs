{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Exinst.Internal.Sum
 ( S1(S1L,S1R)
 , S2(S2L,S2R)
 , S3(S3L,S3R)
 , S4(S4L,S4R)
 ) where

import GHC.Generics (Generic)

#ifdef VERSION_aeson
import Data.Aeson (FromJSON, ToJSON)
#endif

#ifdef VERSION_binary
import qualified Data.Binary as Bin
#endif

#ifdef VERSION_bytes
import qualified Data.Bytes.Serial as By
#endif

#ifdef VERSION_cereal
import qualified Data.Serialize as Cer
#endif

#ifdef VERSION_deepseq
import Control.DeepSeq (NFData)
#endif

#ifdef VERSION_hashable
import Data.Hashable (Hashable)
#endif

#ifdef VERSION_QuickCheck
import qualified Test.QuickCheck as QC
#endif

--------------------------------------------------------------------------------
-- Sums

-- | Like 'Data.Functor.Sum.Sum' from "Data.Functor.Sum", but
-- only intended to be used with kinds other than 'Type'.
--
-- This type is particularly useful when used in combination with 'Some1' as
-- @'Some1' ('S1' l r)@, so as to ensure that @l@ and @r@ are indexed by the
-- same type. Moreover, 'S1' already supports many common instances from
-- @base@, @hashable@, @deepseq@, @aeson@, @bytes@, etc. out of the box, so you
-- can benefit from them as well.
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

--------------------------------------------------------------------------------
#ifdef VERSION_hashable
instance (Hashable (l a1), Hashable (r a1)) => Hashable (S1 l r a1)
instance (Hashable (l a2 a1), Hashable (r a2 a1)) => Hashable (S2 l r a2 a1)
instance (Hashable (l a3 a2 a1), Hashable (r a3 a2 a1)) => Hashable (S3 l r a3 a2 a1)
instance (Hashable (l a4 a3 a2 a1), Hashable (r a4 a3 a2 a1)) => Hashable (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_deepseq
instance (NFData (l a1), NFData (r a1)) => NFData (S1 l r a1)
instance (NFData (l a2 a1), NFData (r a2 a1)) => NFData (S2 l r a2 a1)
instance (NFData (l a3 a2 a1), NFData (r a3 a2 a1)) => NFData (S3 l r a3 a2 a1)
instance (NFData (l a4 a3 a2 a1), NFData (r a4 a3 a2 a1)) => NFData (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_aeson
instance (FromJSON (l a1), FromJSON (r a1)) => FromJSON (S1 l r a1)
instance (FromJSON (l a2 a1), FromJSON (r a2 a1)) => FromJSON (S2 l r a2 a1)
instance (FromJSON (l a3 a2 a1), FromJSON (r a3 a2 a1)) => FromJSON (S3 l r a3 a2 a1)
instance (FromJSON (l a4 a3 a2 a1), FromJSON (r a4 a3 a2 a1)) => FromJSON (S4 l r a4 a3 a2 a1)

instance (ToJSON (l a1), ToJSON (r a1)) => ToJSON (S1 l r a1)
instance (ToJSON (l a2 a1), ToJSON (r a2 a1)) => ToJSON (S2 l r a2 a1)
instance (ToJSON (l a3 a2 a1), ToJSON (r a3 a2 a1)) => ToJSON (S3 l r a3 a2 a1)
instance (ToJSON (l a4 a3 a2 a1), ToJSON (r a4 a3 a2 a1)) => ToJSON (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_bytes
instance (By.Serial (l a1), By.Serial (r a1)) => By.Serial (S1 l r a1)
instance (By.Serial (l a2 a1), By.Serial (r a2 a1)) => By.Serial (S2 l r a2 a1)
instance (By.Serial (l a3 a2 a1), By.Serial (r a3 a2 a1)) => By.Serial (S3 l r a3 a2 a1)
instance (By.Serial (l a4 a3 a2 a1), By.Serial (r a4 a3 a2 a1)) => By.Serial (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_cereal
instance (Cer.Serialize (l a1), Cer.Serialize (r a1)) => Cer.Serialize (S1 l r a1)
instance (Cer.Serialize (l a2 a1), Cer.Serialize (r a2 a1)) => Cer.Serialize (S2 l r a2 a1)
instance (Cer.Serialize (l a3 a2 a1), Cer.Serialize (r a3 a2 a1)) => Cer.Serialize (S3 l r a3 a2 a1)
instance (Cer.Serialize (l a4 a3 a2 a1), Cer.Serialize (r a4 a3 a2 a1)) => Cer.Serialize (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_binary
instance (Bin.Binary (l a1), Bin.Binary (r a1)) => Bin.Binary (S1 l r a1)
instance (Bin.Binary (l a2 a1), Bin.Binary (r a2 a1)) => Bin.Binary (S2 l r a2 a1)
instance (Bin.Binary (l a3 a2 a1), Bin.Binary (r a3 a2 a1)) => Bin.Binary (S3 l r a3 a2 a1)
instance (Bin.Binary (l a4 a3 a2 a1), Bin.Binary (r a4 a3 a2 a1)) => Bin.Binary (S4 l r a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_QuickCheck
instance (QC.Arbitrary (l a1), QC.Arbitrary (r a1)) => QC.Arbitrary (S1 l r a1) where
  arbitrary = QC.oneof [ fmap S1L QC.arbitrary, fmap S1R QC.arbitrary ]
  shrink (S1L l) = S1L <$> QC.shrink l
  shrink (S1R r) = S1R <$> QC.shrink r
instance (QC.Arbitrary (l a2 a1), QC.Arbitrary (r a2 a1)) => QC.Arbitrary (S2 l r a2 a1) where
  arbitrary = QC.oneof [ fmap S2L QC.arbitrary, fmap S2R QC.arbitrary ]
  shrink (S2L l) = S2L <$> QC.shrink l
  shrink (S2R r) = S2R <$> QC.shrink r
instance (QC.Arbitrary (l a3 a2 a1), QC.Arbitrary (r a3 a2 a1)) => QC.Arbitrary (S3 l r a3 a2 a1) where
  arbitrary = QC.oneof [ fmap S3L QC.arbitrary, fmap S3R QC.arbitrary ]
  shrink (S3L l) = S3L <$> QC.shrink l
  shrink (S3R r) = S3R <$> QC.shrink r
instance (QC.Arbitrary (l a4 a3 a2 a1), QC.Arbitrary (r a4 a3 a2 a1)) => QC.Arbitrary (S4 l r a4 a3 a2 a1) where
  arbitrary = QC.oneof [ fmap S4L QC.arbitrary, fmap S4R QC.arbitrary ]
  shrink (S4L l) = S4L <$> QC.shrink l
  shrink (S4R r) = S4R <$> QC.shrink r
#endif
