{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Binary as Bin
import qualified Data.Bytes.Get as Bytes
import qualified Data.Bytes.Put as Bytes
import qualified Data.Bytes.Serial as Bytes
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.Kind (Type)
import qualified Data.Serialize as Cer
import qualified GHC.Generics as G
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC

import Exinst
import Exinst.Bytes ()
import Exinst.Cereal ()

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

--------------------------------------------------------------------------------

data family X1 :: Bool -> Type
data instance X1 'False = XF1 | XF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X1 'True = XT1 | XT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)

data family X2 :: Bool -> Bool -> Type
data instance X2 'False 'False = XFF1 | XFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X2 'False 'True = XFT1 | XFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X2 'True 'False = XTF1 | XTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X2 'True 'True = XTT1 | XTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)

data family X3 :: Bool -> Bool -> Bool -> Type
data instance X3 'False 'False 'False = XFFF1 | XFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'False 'False 'True = XFFT1 | XFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'False 'True 'False = XFTF1 | XFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'False 'True 'True = XFTT1 | XFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'True 'False 'False = XTFF1 | XTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'True 'False 'True = XTFT1 | XTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'True 'True 'False = XTTF1 | XTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X3 'True 'True 'True = XTTT1 | XTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)

data family X4 :: Bool -> Bool -> Bool -> Bool -> Type
data instance X4 'False 'False 'False 'False = XFFFF1 | XFFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'False 'False 'True = XFFFT1 | XFFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'False 'True 'False = XFFTF1 | XFFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'False 'True 'True = XFFTT1 | XFFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'True 'False 'False = XFTFF1 | XFTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'True 'False 'True = XFTFT1 | XFTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'True 'True 'False = XFTTF1 | XFTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'False 'True 'True 'True = XFTTT1 | XFTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'False 'False 'False = XTFFF1 | XTFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'False 'False 'True = XTFFT1 | XTFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'False 'True 'False = XTFTF1 | XTFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'False 'True 'True = XTFTT1 | XTFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'True 'False 'False = XTTFF1 | XTTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'True 'False 'True = XTTFT1 | XTTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'True 'True 'False = XTTTF1 | XTTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)
data instance X4 'True 'True 'True 'True = XTTTT1 | XTTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Bin.Binary)

#define INSTANCETRON(c) \
  instance c (X1 'False); \
  instance c (X1 'True); \
  instance c (X2 'False 'False); \
  instance c (X2 'False 'True); \
  instance c (X2 'True 'False); \
  instance c (X2 'True 'True); \
  instance c (X3 'False 'False 'False); \
  instance c (X3 'False 'False 'True); \
  instance c (X3 'False 'True 'False); \
  instance c (X3 'False 'True 'True); \
  instance c (X3 'True 'False 'False); \
  instance c (X3 'True 'False 'True); \
  instance c (X3 'True 'True 'False); \
  instance c (X3 'True 'True 'True); \
  instance c (X4 'False 'False 'False 'False); \
  instance c (X4 'False 'False 'False 'True); \
  instance c (X4 'False 'False 'True 'False); \
  instance c (X4 'False 'False 'True 'True); \
  instance c (X4 'False 'True 'False 'False); \
  instance c (X4 'False 'True 'False 'True); \
  instance c (X4 'False 'True 'True 'False); \
  instance c (X4 'False 'True 'True 'True); \
  instance c (X4 'True 'False 'False 'False); \
  instance c (X4 'True 'False 'False 'True); \
  instance c (X4 'True 'False 'True 'False); \
  instance c (X4 'True 'False 'True 'True); \
  instance c (X4 'True 'True 'False 'False); \
  instance c (X4 'True 'True 'False 'True); \
  instance c (X4 'True 'True 'True 'False); \
  instance c (X4 'True 'True 'True 'True)

--------------------------------------------------------------------------------
-- Arbitrary instances

instance QC.Arbitrary (X1 'False) where arbitrary = QC.oneof [ pure XF1, fmap XF2 QC.arbitrary ]
instance QC.Arbitrary (X1 'True) where arbitrary = QC.oneof [ pure XT1, fmap XT2 QC.arbitrary ]

instance QC.Arbitrary (X2 'False 'False) where arbitrary = QC.oneof [ pure XFF1, fmap XFF2 QC.arbitrary ]
instance QC.Arbitrary (X2 'False 'True) where arbitrary = QC.oneof [ pure XFT1, fmap XFT2 QC.arbitrary ]
instance QC.Arbitrary (X2 'True 'False) where arbitrary = QC.oneof [ pure XTF1, fmap XTF2 QC.arbitrary ]
instance QC.Arbitrary (X2 'True 'True) where arbitrary = QC.oneof [ pure XTT1, fmap XTT2 QC.arbitrary ]

instance QC.Arbitrary (X3 'False 'False 'False) where arbitrary = QC.oneof [ pure XFFF1, fmap XFFF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'False 'False 'True) where arbitrary = QC.oneof [ pure XFFT1, fmap XFFT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'False 'True 'False) where arbitrary = QC.oneof [ pure XFTF1, fmap XFTF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'False 'True 'True) where arbitrary = QC.oneof [ pure XFTT1, fmap XFTT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'True 'False 'False) where arbitrary = QC.oneof [ pure XTFF1, fmap XTFF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'True 'False 'True) where arbitrary = QC.oneof [ pure XTFT1, fmap XTFT2 QC.arbitrary ]
instance QC.Arbitrary (X3 'True 'True 'False) where arbitrary = QC.oneof [ pure XTTF1, fmap XTTF2 QC.arbitrary ]
instance QC.Arbitrary (X3 'True 'True 'True) where arbitrary = QC.oneof [ pure XTTT1, fmap XTTT2 QC.arbitrary ]

instance QC.Arbitrary (X4 'False 'False 'False 'False) where arbitrary = QC.oneof [ pure XFFFF1, fmap XFFFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'False 'False 'True) where arbitrary = QC.oneof [ pure XFFFT1, fmap XFFFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'False 'True 'False) where arbitrary = QC.oneof [ pure XFFTF1, fmap XFFTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'False 'True 'True) where arbitrary = QC.oneof [ pure XFFTT1, fmap XFFTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'True 'False 'False) where arbitrary = QC.oneof [ pure XFTFF1, fmap XFTFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'True 'False 'True) where arbitrary = QC.oneof [ pure XFTFT1, fmap XFTFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'True 'True 'False) where arbitrary = QC.oneof [ pure XFTTF1, fmap XFTTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'False 'True 'True 'True) where arbitrary = QC.oneof [ pure XFTTT1, fmap XFTTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'False 'False 'False) where arbitrary = QC.oneof [ pure XTFFF1, fmap XTFFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'False 'False 'True) where arbitrary = QC.oneof [ pure XTFFT1, fmap XTFFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'False 'True 'False) where arbitrary = QC.oneof [ pure XTFTF1, fmap XTFTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'False 'True 'True) where arbitrary = QC.oneof [ pure XTFTT1, fmap XTFTT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'True 'False 'False) where arbitrary = QC.oneof [ pure XTTFF1, fmap XTTFF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'True 'False 'True) where arbitrary = QC.oneof [ pure XTTFT1, fmap XTTFT2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'True 'True 'False) where arbitrary = QC.oneof [ pure XTTTF1, fmap XTTTF2 QC.arbitrary ]
instance QC.Arbitrary (X4 'True 'True 'True 'True) where arbitrary = QC.oneof [ pure XTTTT1, fmap XTTTT2 QC.arbitrary ]

--------------------------------------------------------------------------------

tt :: Tasty.TestTree
tt =
  Tasty.testGroup "main"
  [ tt_id "Identity through Bytes's Serial" id_bytes
  , tt_id "Identity from Binary's Binary to Bytes's Serial" id_binary_to_bytes
  , tt_id "Identity from Bytes's Serial to Binary's Binary" id_bytes_to_binary
  , tt_id "Identity from Cereal's Serialize to Bytes's Serial" id_cereal_to_bytes
  , tt_id "Identity from Bytes's Serial to Cereal's Serialize" id_bytes_to_cereal
  , tt_id "Identity through Cereal's Serialize" id_cereal
  , tt_id "Identity from Binary's Binary to Cereal's Serialize" id_binary_to_cereal
  , tt_id "Identity from Cereal's Serialize to Binary's Binary" id_cereal_to_binary
  ]

type MegaCtx a = (Bin.Binary a, Bytes.Serial a, Cer.Serialize a)

tt_id
  :: String
  -> (forall a. MegaCtx a => a -> Maybe a)
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

--------------------------------------------------------------------------------

INSTANCETRON(Bytes.Serial)
id_bytes :: Bytes.Serial a => a -> Maybe a
id_bytes = \a ->
  case Bytes.runGetS Bytes.deserialize (Bytes.runPutS (Bytes.serialize a)) of
     Left _ -> Nothing
     Right a' -> Just a'

id_binary_to_bytes :: (Bin.Binary a, Bytes.Serial a) => a -> Maybe a
id_binary_to_bytes = \a ->
   case Bytes.runGetS Bytes.deserialize (BSL.toStrict (Bin.encode a)) of
     Left _ -> Nothing
     Right a' -> Just a'

id_bytes_to_binary :: (Bytes.Serial a, Bin.Binary a) => a -> Maybe a
id_bytes_to_binary = \a ->
   case Bin.decodeOrFail (Bytes.runPutL (Bytes.serialize a)) of
      Right (z,_,a') | BSL.null z -> Just a'
      _ -> Nothing

id_binary :: Bin.Binary a => a -> Maybe a
id_binary = \a ->
  case Bin.decodeOrFail (Bin.encode a) of
      Right (z,_,a') | BSL.null z -> Just a'
      _ -> Nothing

INSTANCETRON(Cer.Serialize)
id_cereal :: Cer.Serialize a => a -> Maybe a
id_cereal = \a ->
  case Cer.decodeLazy (Cer.encodeLazy a) of
     Right a' -> Just a'
     Left _ -> Nothing

id_binary_to_cereal :: (Bin.Binary a, Cer.Serialize a) => a -> Maybe a
id_binary_to_cereal = \a ->
  case Cer.decodeLazy (Bin.encode a) of
     Right a' -> Just a'
     Left _ -> Nothing

id_cereal_to_binary :: (Bin.Binary a, Cer.Serialize a) => a -> Maybe a
id_cereal_to_binary = \a ->
   case Bin.decodeOrFail (Cer.encodeLazy a) of
      Right (z,_,a') | BSL.null z -> Just a'
      _ -> Nothing

id_cereal_to_bytes :: (Cer.Serialize a, Bytes.Serial a) => a -> Maybe a
id_cereal_to_bytes = \a ->
   case Bytes.runGetS Bytes.deserialize (Cer.encode a) of
     Left _ -> Nothing
     Right a' -> Just a'

id_bytes_to_cereal :: (Bytes.Serial a, Cer.Serialize a) => a -> Maybe a
id_bytes_to_cereal = \a ->
  case Cer.decodeLazy (Bytes.runPutL (Bytes.serialize a)) of
     Right a' -> Just a'
     Left _ -> Nothing

