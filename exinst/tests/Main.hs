{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq (NFData(rnf))
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL
import Data.Singletons
import Data.Singletons.Decide
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Int (Int32)
import Data.Kind (Type)
import qualified GHC.Generics as G
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import Text.Read (readMaybe)

import Exinst

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

--------------------------------------------------------------------------------

data Foo = Bar | Qux
  deriving (Eq, Ord, Show, Read)

instance QC.Arbitrary Foo where
  arbitrary = QC.oneof [pure Bar, pure Qux]

instance Hashable Foo where
  hashWithSalt s Bar = hashWithSalt s False
  hashWithSalt s Qux = hashWithSalt s True

instance Bin.Binary Foo where
  put Bar = Bin.put False
  put Qux = Bin.put True
  get = Bin.get >>= \x -> pure $ case x of False -> Bar
                                           True -> Qux
         
data SFoo (x :: Foo) where
  SBar :: SFoo 'Bar
  SQux :: SFoo 'Qux

type instance Sing = SFoo

instance SingKind Foo where
  type Demote Foo = Foo
  fromSing SBar = Bar
  fromSing SQux = Qux
  toSing Bar = SomeSing SBar
  toSing Qux = SomeSing SQux

instance SingI 'Bar where 
  sing = SBar
instance SingI 'Qux where 
  sing = SQux

instance SDecide Foo where
  SBar %~ SBar = Proved Refl
  SQux %~ SQux = Proved Refl
  _ %~ _ = Disproved undefined

instance forall c.
  (c 'Bar, c 'Qux
  ) => Dict0 (c :: Foo -> Constraint) where
  dict0 = \x -> case x of { SBar -> Dict; SQux -> Dict }

instance forall k0 c f.
  ( c (f 'Bar), c (f 'Qux)
  ) => Dict1 c (f :: Foo -> k0) where
  dict1 = \x -> case x of { SBar -> Dict; SQux -> Dict }

instance forall k1 k0 c f.
  ( Dict1 c (f 'Bar), Dict1 c (f 'Qux)
  ) => Dict2 c (f :: Foo -> k1 -> k0) where
  dict2 = \x -> case x of { SBar -> dict1; SQux -> dict1 }

instance forall k2 k1 k0 c f.
  ( Dict2 c (f 'Bar), Dict2 c (f 'Qux)
  ) => Dict3 c (f :: Foo -> k2 -> k1 -> k0) where
  dict3 = \x -> case x of { SBar -> dict2; SQux -> dict2 }

instance forall k3 k2 k1 k0 c f.
  ( Dict3 c (f 'Bar), Dict3 c (f 'Qux)
  ) => Dict4 c (f :: Foo -> k3 -> k2 -> k1 -> k0) where
  dict4 = \x -> case x of { SBar -> dict3; SQux -> dict3 }

--------------------------------------------------------------------------------

data family X1 :: Foo -> Type
data instance X1 'Bar = XF1 | XF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X1 'Qux = XT1 | XT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)

data family X2 :: Foo -> Foo -> Type
data instance X2 'Bar 'Bar = XFF1 | XFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X2 'Bar 'Qux = XFT1 | XFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X2 'Qux 'Bar = XTF1 | XTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X2 'Qux 'Qux = XTT1 | XTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)

data family X3 :: Foo -> Foo -> Foo -> Type
data instance X3 'Bar 'Bar 'Bar = XFFF1 | XFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Bar 'Bar 'Qux = XFFT1 | XFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Bar 'Qux 'Bar = XFTF1 | XFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Bar 'Qux 'Qux = XFTT1 | XFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Qux 'Bar 'Bar = XTFF1 | XTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Qux 'Bar 'Qux = XTFT1 | XTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Qux 'Qux 'Bar = XTTF1 | XTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X3 'Qux 'Qux 'Qux = XTTT1 | XTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)

data family X4 :: Foo -> Foo -> Foo -> Foo -> Type
data instance X4 'Bar 'Bar 'Bar 'Bar = XFFFF1 | XFFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Bar 'Bar 'Qux = XFFFT1 | XFFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Bar 'Qux 'Bar = XFFTF1 | XFFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Bar 'Qux 'Qux = XFFTT1 | XFFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Qux 'Bar 'Bar = XFTFF1 | XFTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Qux 'Bar 'Qux = XFTFT1 | XFTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Qux 'Qux 'Bar = XFTTF1 | XFTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Bar 'Qux 'Qux 'Qux = XFTTT1 | XFTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Bar 'Bar 'Bar = XTFFF1 | XTFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Bar 'Bar 'Qux = XTFFT1 | XTFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Bar 'Qux 'Bar = XTFTF1 | XTFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Bar 'Qux 'Qux = XTFTT1 | XTFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Qux 'Bar 'Bar = XTTFF1 | XTTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Qux 'Bar 'Qux = XTTFT1 | XTTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Qux 'Qux 'Bar = XTTTF1 | XTTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)
data instance X4 'Qux 'Qux 'Qux 'Qux = XTTTT1 | XTTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary, NFData)

#define INSTANCETRON(c) \
  instance c (X1 'Bar); \
  instance c (X1 'Qux); \
  instance c (X2 'Bar 'Bar); \
  instance c (X2 'Bar 'Qux); \
  instance c (X2 'Qux 'Bar); \
  instance c (X2 'Qux 'Qux); \
  instance c (X3 'Bar 'Bar 'Bar); \
  instance c (X3 'Bar 'Bar 'Qux); \
  instance c (X3 'Bar 'Qux 'Bar); \
  instance c (X3 'Bar 'Qux 'Qux); \
  instance c (X3 'Qux 'Bar 'Bar); \
  instance c (X3 'Qux 'Bar 'Qux); \
  instance c (X3 'Qux 'Qux 'Bar); \
  instance c (X3 'Qux 'Qux 'Qux); \
  instance c (X4 'Bar 'Bar 'Bar 'Bar); \
  instance c (X4 'Bar 'Bar 'Bar 'Qux); \
  instance c (X4 'Bar 'Bar 'Qux 'Bar); \
  instance c (X4 'Bar 'Bar 'Qux 'Qux); \
  instance c (X4 'Bar 'Qux 'Bar 'Bar); \
  instance c (X4 'Bar 'Qux 'Bar 'Qux); \
  instance c (X4 'Bar 'Qux 'Qux 'Bar); \
  instance c (X4 'Bar 'Qux 'Qux 'Qux); \
  instance c (X4 'Qux 'Bar 'Bar 'Bar); \
  instance c (X4 'Qux 'Bar 'Bar 'Qux); \
  instance c (X4 'Qux 'Bar 'Qux 'Bar); \
  instance c (X4 'Qux 'Bar 'Qux 'Qux); \
  instance c (X4 'Qux 'Qux 'Bar 'Bar); \
  instance c (X4 'Qux 'Qux 'Bar 'Qux); \
  instance c (X4 'Qux 'Qux 'Qux 'Bar); \
  instance c (X4 'Qux 'Qux 'Qux 'Qux)

--------------------------------------------------------------------------------
-- Arbitrary instances

instance QC.Arbitrary (X1 'Bar) where arbitrary = QC.oneof [ pure XF1, fmap XF2 QC.arbitrary ]
instance QC.Arbitrary (X1 'Qux) where arbitrary = QC.oneof [ pure XT1, fmap XT2 QC.arbitrary ]

instance QC.Arbitrary (X2 'Bar 'Bar) where arbitrary = QC.oneof [ pure XFF1, fmap XFF2 QC.arbitrary ]
instance QC.Arbitrary (X2 'Bar 'Qux) where arbitrary = QC.oneof [ pure XFT1, fmap XFT2 QC.arbitrary ]
instance QC.Arbitrary (X2 'Qux 'Bar) where arbitrary = QC.oneof [ pure XTF1, fmap XTF2 QC.arbitrary ]
instance QC.Arbitrary (X2 'Qux 'Qux) where arbitrary = QC.oneof [ pure XTT1, fmap XTT2 QC.arbitrary ]

instance QC.Arbitrary (X3 'Bar 'Bar 'Bar) where arbitrary = QC.oneof [ pure XFFF1, fmap XFFF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Bar 'Bar 'Qux) where arbitrary = QC.oneof [ pure XFFT1, fmap XFFT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Bar 'Qux 'Bar) where arbitrary = QC.oneof [ pure XFTF1, fmap XFTF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Bar 'Qux 'Qux) where arbitrary = QC.oneof [ pure XFTT1, fmap XFTT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Qux 'Bar 'Bar) where arbitrary = QC.oneof [ pure XTFF1, fmap XTFF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Qux 'Bar 'Qux) where arbitrary = QC.oneof [ pure XTFT1, fmap XTFT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Qux 'Qux 'Bar) where arbitrary = QC.oneof [ pure XTTF1, fmap XTTF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'Qux 'Qux 'Qux) where arbitrary = QC.oneof [ pure XTTT1, fmap XTTT2 QC.arbitrary ]

instance QC.Arbitrary (X4 'Bar 'Bar 'Bar 'Bar) where arbitrary = QC.oneof [ pure XFFFF1, fmap XFFFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Bar 'Bar 'Qux) where arbitrary = QC.oneof [ pure XFFFT1, fmap XFFFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Bar 'Qux 'Bar) where arbitrary = QC.oneof [ pure XFFTF1, fmap XFFTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Bar 'Qux 'Qux) where arbitrary = QC.oneof [ pure XFFTT1, fmap XFFTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Qux 'Bar 'Bar) where arbitrary = QC.oneof [ pure XFTFF1, fmap XFTFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Qux 'Bar 'Qux) where arbitrary = QC.oneof [ pure XFTFT1, fmap XFTFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Qux 'Qux 'Bar) where arbitrary = QC.oneof [ pure XFTTF1, fmap XFTTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Bar 'Qux 'Qux 'Qux) where arbitrary = QC.oneof [ pure XFTTT1, fmap XFTTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Bar 'Bar 'Bar) where arbitrary = QC.oneof [ pure XTFFF1, fmap XTFFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Bar 'Bar 'Qux) where arbitrary = QC.oneof [ pure XTFFT1, fmap XTFFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Bar 'Qux 'Bar) where arbitrary = QC.oneof [ pure XTFTF1, fmap XTFTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Bar 'Qux 'Qux) where arbitrary = QC.oneof [ pure XTFTT1, fmap XTFTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Qux 'Bar 'Bar) where arbitrary = QC.oneof [ pure XTTFF1, fmap XTTFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Qux 'Bar 'Qux) where arbitrary = QC.oneof [ pure XTTFT1, fmap XTTFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Qux 'Qux 'Bar) where arbitrary = QC.oneof [ pure XTTTF1, fmap XTTTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'Qux 'Qux 'Qux 'Qux) where arbitrary = QC.oneof [ pure XTTTT1, fmap XTTTT2 QC.arbitrary ]

--------------------------------------------------------------------------------

tt :: Tasty.TestTree
tt =
  Tasty.testGroup "main"
  [ tt_nfdata
  , tt_id "Identity through Show/Read" id_show_read
  , tt_id "Identity through Binary's Binary" id_binary
  ]

tt_id
  :: String
  -> (forall a. (Show a, Read a, Hashable a, Bin.Binary a) => a -> Maybe a)
  -- ^ It's easier to put all the constraints here in the 'MegaCtx' monster.
  -> Tasty.TestTree
tt_id = \title id' -> Tasty.testGroup title
  [ QC.testProperty "Some1 X1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 X1) -> Just x === id' x
  , QC.testProperty "Some2 X2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 X2) -> Just x === id' x
  , QC.testProperty "Some3 X3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 X3) -> Just x === id' x
  , QC.testProperty "Some4 X4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 X4) -> Just x === id' x
  , QC.testProperty "Some1 (P1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (P1 X1 X1)) -> Just x === id' x
  , QC.testProperty "Some2 (P2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (P2 X2 X2)) -> Just x === id' x
  , QC.testProperty "Some3 (P3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (P3 X3 X3)) -> Just x === id' x
  , QC.testProperty "Some4 (P4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (P4 X4 X4)) -> Just x === id' x
  , QC.testProperty "Some1 (S1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (S1 X1 X1)) -> Just x === id' x
  , QC.testProperty "Some2 (S2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (S2 X2 X2)) -> Just x === id' x
  , QC.testProperty "Some3 (S3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (S3 X3 X3)) -> Just x === id' x
  , QC.testProperty "Some4 (S4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (S4 X4 X4)) -> Just x === id' x
  ]

tt_nfdata :: Tasty.TestTree
tt_nfdata = Tasty.testGroup "NFData"
  [ QC.testProperty "Some1 X1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 X1) -> () === rnf x
  , QC.testProperty "Some2 X2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 X2) -> () === rnf x
  , QC.testProperty "Some3 X3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 X3) -> () === rnf x
  , QC.testProperty "Some4 X4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 X4) -> () === rnf x
  , QC.testProperty "Some1 (P1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (P1 X1 X1)) -> () === rnf x
  , QC.testProperty "Some2 (P2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (P2 X2 X2)) -> () === rnf x
  , QC.testProperty "Some3 (P3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (P3 X3 X3)) -> () === rnf x
  , QC.testProperty "Some4 (P4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (P4 X4 X4)) -> () === rnf x
  , QC.testProperty "Some1 (S1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (S1 X1 X1)) -> () === rnf x
  , QC.testProperty "Some2 (S2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (S2 X2 X2)) -> () === rnf x
  , QC.testProperty "Some3 (S3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (S3 X3 X3)) -> () === rnf x
  , QC.testProperty "Some4 (S4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (S4 X4 X4)) -> () === rnf x
  ]

INSTANCETRON(Hashable)

tt_hashable :: Tasty.TestTree
tt_hashable = Tasty.testGroup "Hashable"
  [ QC.testProperty "Some1 X1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 X1) -> () === (hash x `seq` ())
  , QC.testProperty "Some2 X2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 X2) -> () === (hash x `seq` ())
  , QC.testProperty "Some3 X3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 X3) -> () === (hash x `seq` ())
  , QC.testProperty "Some4 X4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 X4) -> () === (hash x `seq` ())
  , QC.testProperty "Some1 (P1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (P1 X1 X1)) -> () === (hash x `seq` ())
  , QC.testProperty "Some2 (P2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (P2 X2 X2)) -> () === (hash x `seq` ())
  , QC.testProperty "Some3 (P3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (P3 X3 X3)) -> () === (hash x `seq` ())
  , QC.testProperty "Some4 (P4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (P4 X4 X4)) -> () === (hash x `seq` ())
  , QC.testProperty "Some1 (S1 X1 X1)" $
      QC.forAll QC.arbitrary $ \(x :: Some1 (S1 X1 X1)) -> () === (hash x `seq` ())
  , QC.testProperty "Some2 (S2 X2 X2)" $
      QC.forAll QC.arbitrary $ \(x :: Some2 (S2 X2 X2)) -> () === (hash x `seq` ())
  , QC.testProperty "Some3 (S3 X3 X3)" $
      QC.forAll QC.arbitrary $ \(x :: Some3 (S3 X3 X3)) -> () === (hash x `seq` ())
  , QC.testProperty "Some4 (S4 X4 X4)" $
      QC.forAll QC.arbitrary $ \(x :: Some4 (S4 X4 X4)) -> () === (hash x `seq` ())
  ]

--------------------------------------------------------------------------------

id_show_read :: (Show a, Read a) => a -> Maybe a
id_show_read = readMaybe . show

id_binary :: Bin.Binary a => a -> Maybe a
id_binary = \a ->
  case Bin.decodeOrFail (Bin.encode a) of
      Right (z,_,a') | BSL.null z -> Just a'
      _ -> Nothing

