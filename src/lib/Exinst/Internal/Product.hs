{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}

module Exinst.Internal.Product
 ( P1(P1)
 , P2(P2)
 , P3(P3)
 , P4(P4)
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
-- Products

-- | Like 'Data.Functor.Product.Product' from "Data.Functor.Product", but
-- only intended to be used with kinds other than 'Type'.
--
-- This type is particularly useful when used in combination with 'Some1' as
-- @'Some1' ('P1' f g)@, so as to ensure that @f@ and @g@ are indexed by the
-- same type. Moreover, 'P1' already supports many common instances from
-- @base@, @hashable@, @deepseq@, @aeson@, @bytes@, etc. out of the box, so you
-- can benefit from them as well.
data P1 f g (a1 :: k1)
  = P1 (f a1) (g a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @f@ and @g@ taking two type indexes.
data P2 f g (a2 :: k2) (a1 :: k1)
  = P2 (f a2 a1) (g a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @f@ and @g@ taking three type indexes.
data P3 f g (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = P3 (f a3 a2 a1) (g a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

-- | Like 'P1', but for @f@ and @g@ taking four type indexes.
data P4 f g (a4 :: k4) (a3 :: k3) (a2 :: k2) (a1 :: k1)
  = P4 (f a4 a3 a2 a1) (g a4 a3 a2 a1)
  deriving (Eq, Show, Read, Ord, Generic)

--------------------------------------------------------------------------------
#ifdef VERSION_hashable
instance (Hashable (f a1), Hashable (g a1)) => Hashable (P1 f g a1)
instance (Hashable (f a2 a1), Hashable (g a2 a1)) => Hashable (P2 f g a2 a1)
instance (Hashable (f a3 a2 a1), Hashable (g a3 a2 a1)) => Hashable (P3 f g a3 a2 a1)
instance (Hashable (f a4 a3 a2 a1), Hashable (g a4 a3 a2 a1)) => Hashable (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_deepseq
instance (NFData (f a1), NFData (g a1)) => NFData (P1 f g a1)
instance (NFData (f a2 a1), NFData (g a2 a1)) => NFData (P2 f g a2 a1)
instance (NFData (f a3 a2 a1), NFData (g a3 a2 a1)) => NFData (P3 f g a3 a2 a1)
instance (NFData (f a4 a3 a2 a1), NFData (g a4 a3 a2 a1)) => NFData (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_aeson
instance (FromJSON (f a1), FromJSON (g a1)) => FromJSON (P1 f g a1)
instance (FromJSON (f a2 a1), FromJSON (g a2 a1)) => FromJSON (P2 f g a2 a1)
instance (FromJSON (f a3 a2 a1), FromJSON (g a3 a2 a1)) => FromJSON (P3 f g a3 a2 a1)
instance (FromJSON (f a4 a3 a2 a1), FromJSON (g a4 a3 a2 a1)) => FromJSON (P4 f g a4 a3 a2 a1)

instance (ToJSON (f a1), ToJSON (g a1)) => ToJSON (P1 f g a1)
instance (ToJSON (f a2 a1), ToJSON (g a2 a1)) => ToJSON (P2 f g a2 a1)
instance (ToJSON (f a3 a2 a1), ToJSON (g a3 a2 a1)) => ToJSON (P3 f g a3 a2 a1)
instance (ToJSON (f a4 a3 a2 a1), ToJSON (g a4 a3 a2 a1)) => ToJSON (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_bytes
instance (By.Serial (f a1), By.Serial (g a1)) => By.Serial (P1 f g a1)
instance (By.Serial (f a2 a1), By.Serial (g a2 a1)) => By.Serial (P2 f g a2 a1)
instance (By.Serial (f a3 a2 a1), By.Serial (g a3 a2 a1)) => By.Serial (P3 f g a3 a2 a1)
instance (By.Serial (f a4 a3 a2 a1), By.Serial (g a4 a3 a2 a1)) => By.Serial (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_cereal
instance (Cer.Serialize (f a1), Cer.Serialize (g a1)) => Cer.Serialize (P1 f g a1)
instance (Cer.Serialize (f a2 a1), Cer.Serialize (g a2 a1)) => Cer.Serialize (P2 f g a2 a1)
instance (Cer.Serialize (f a3 a2 a1), Cer.Serialize (g a3 a2 a1)) => Cer.Serialize (P3 f g a3 a2 a1)
instance (Cer.Serialize (f a4 a3 a2 a1), Cer.Serialize (g a4 a3 a2 a1)) => Cer.Serialize (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_binary
instance (Bin.Binary (f a1), Bin.Binary (g a1)) => Bin.Binary (P1 f g a1)
instance (Bin.Binary (f a2 a1), Bin.Binary (g a2 a1)) => Bin.Binary (P2 f g a2 a1)
instance (Bin.Binary (f a3 a2 a1), Bin.Binary (g a3 a2 a1)) => Bin.Binary (P3 f g a3 a2 a1)
instance (Bin.Binary (f a4 a3 a2 a1), Bin.Binary (g a4 a3 a2 a1)) => Bin.Binary (P4 f g a4 a3 a2 a1)
#endif

--------------------------------------------------------------------------------
#ifdef VERSION_QuickCheck
instance (QC.Arbitrary (f a1), QC.Arbitrary (g a1)) => QC.Arbitrary (P1 f g a1) where
  arbitrary = P1 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P1 x y) = P1 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (f a2 a1), QC.Arbitrary (g a2 a1)) => QC.Arbitrary (P2 f g a2 a1) where
  arbitrary = P2 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P2 x y) = P2 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (f a3 a2 a1), QC.Arbitrary (g a3 a2 a1)) => QC.Arbitrary (P3 f g a3 a2 a1) where
  arbitrary = P3 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P3 x y) = P3 <$> QC.shrink x <*> QC.shrink y
instance (QC.Arbitrary (f a4 a3 a2 a1), QC.Arbitrary (g a4 a3 a2 a1)) => QC.Arbitrary (P4 f g a4 a3 a2 a1) where
  arbitrary = P4 <$> QC.arbitrary <*> QC.arbitrary
  shrink (P4 x y) = P4 <$> QC.shrink x <*> QC.shrink y
#endif
