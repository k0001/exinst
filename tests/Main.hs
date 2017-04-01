{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.DeepSeq (NFData(rnf))
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Bytes.Get as Bytes
import qualified Data.Bytes.Put as Bytes
import qualified Data.Bytes.Serial as Bytes
import Data.Hashable (Hashable(hash))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Data.Serialize as Cer
import qualified GHC.Generics as G
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import Text.Read (readMaybe)

import Data.Singletons (SingKind, Sing, DemoteRep, withSomeSing)

import Exinst

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

--------------------------------------------------------------------------------

tt :: Tasty.TestTree
tt =
  Tasty.testGroup "main"
  [ tt_id "Identity through Show/Read" id_show_read
  , tt_id "Identity through GHC's Generic" id_generic
  , tt_id "Identity through Aeson's ToJSON/FromJSON" id_aeson
  , tt_id "Identity through Bytes's Serial" id_bytes
  , tt_id "Identity through Cereal's Serialize" id_cereal
  , tt_id "Identity through Binary's Binary" id_binary
  , tt_id "Identity from Cereal's Serialize to Binary's Binary" id_cereal_to_binary
  , tt_id "Identity from Cereal's Serialize to Bytes's Serial" id_cereal_to_bytes
  , tt_id "Identity from Binary's Binary to Cereal's Serialize" id_binary_to_cereal
  , tt_id "Identity from Binary's Binary to Bytes's Serial" id_binary_to_bytes
  , tt_id "Identity from Bytes's Serial to Binary's Binary" id_bytes_to_binary
  , tt_id "Identity from Bytes's Serial to Cereal's Serialize" id_bytes_to_cereal
  , tt_nfdata
  ]

tt_id
  :: String
  -> (forall a.
        ( G.Generic a, Aeson.FromJSON a, Aeson.ToJSON a
        , Bytes.Serial a, Bin.Binary a, Cer.Serialize a
        , Show a, Read a
        ) => a -> Maybe a
     ) -- ^ It's easier to put all the constraints here.
  -> Tasty.TestTree
tt_id = \title id' -> Tasty.testGroup title
  [ QC.testProperty "Some1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 Foo1) -> Just x === id' x
  , QC.testProperty "Some2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 Foo2) -> Just x === id' x
  , QC.testProperty "Some3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 Foo3) -> Just x === id' x
  , QC.testProperty "Some4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 Foo4) -> Just x === id' x
  ]

tt_nfdata :: Tasty.TestTree
tt_nfdata = Tasty.testGroup "NFData"
  [ QC.testProperty "Some1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 Foo1) -> () === rnf x
  , QC.testProperty "Some2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 Foo2) -> () === rnf x
  , QC.testProperty "Some3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 Foo3) -> () === rnf x
  , QC.testProperty "Some4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 Foo4) -> () === rnf x
  ]

tt_hashable :: Tasty.TestTree
tt_hashable = Tasty.testGroup "Hashable"
  [ QC.testProperty "Some1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 Foo1) -> () === (hash x `seq` ())
  , QC.testProperty "Some2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 Foo2) -> () === (hash x `seq` ())
  , QC.testProperty "Some3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 Foo3) -> () === (hash x `seq` ())
  , QC.testProperty "Some4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 Foo4) -> () === (hash x `seq` ())
  ]

--------------------------------------------------------------------------------

id_show_read :: (Show a, Read a) => a -> Maybe a
id_show_read = readMaybe . show

id_generic :: G.Generic a => a -> Maybe a
id_generic = Just . G.to . G.from

id_aeson :: (Aeson.FromJSON a, Aeson.ToJSON a) => a -> Maybe a
id_aeson = Aeson.decode . Aeson.encode

id_bytes :: Bytes.Serial a => a -> Maybe a
id_bytes = \a ->
  case Bytes.runGetS Bytes.deserialize (Bytes.runPutS (Bytes.serialize a)) of
     Left _ -> Nothing
     Right a' -> Just a'

id_binary :: Bin.Binary a => a -> Maybe a
id_binary = \a ->
  case Bin.decodeOrFail (Bin.encode a) of
      Right (z,_,a') | BSL.null z -> Just a'
      _ -> Nothing

id_cereal :: Cer.Serialize a => a -> Maybe a
id_cereal = \a ->
  case Cer.decodeLazy (Cer.encodeLazy a) of
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

id_binary_to_cereal :: (Bin.Binary a, Cer.Serialize a) => a -> Maybe a
id_binary_to_cereal = \a ->
  case Cer.decodeLazy (Bin.encode a) of
     Right a' -> Just a'
     Left _ -> Nothing

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

id_bytes_to_cereal :: (Bytes.Serial a, Cer.Serialize a) => a -> Maybe a
id_bytes_to_cereal = \a ->
  case Cer.decodeLazy (Bytes.runPutL (Bytes.serialize a)) of
     Right a' -> Just a'
     Left _ -> Nothing

--------------------------------------------------------------------------------

data family Foo1 :: Bool -> Type
data instance Foo1 'False = F1 | F2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo1 'True = T1 | T2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)

data family Foo2 :: Bool -> Bool -> Type
data instance Foo2 'False 'False = FF1 | FF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo2 'False 'True = FT1 | FT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo2 'True 'False = TF1 | TF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo2 'True 'True = TT1 | TT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)

data family Foo3 :: Bool -> Bool -> Bool -> Type
data instance Foo3 'False 'False 'False = FFF1 | FFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'False 'False 'True = FFT1 | FFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'False 'True 'False = FTF1 | FTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'False 'True 'True = FTT1 | FTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'True 'False 'False = TFF1 | TFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'True 'False 'True = TFT1 | TFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'True 'True 'False = TTF1 | TTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo3 'True 'True 'True = TTT1 | TTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)

data family Foo4 :: Bool -> Bool -> Bool -> Bool -> Type
data instance Foo4 'False 'False 'False 'False = FFFF1 | FFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'False 'False 'True = FFFT1 | FFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'False 'True 'False = FFTF1 | FFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'False 'True 'True = FFTT1 | FFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'True 'False 'False = FTFF1 | FTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'True 'False 'True = FTFT1 | FTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'True 'True 'False = FTTF1 | FTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'False 'True 'True 'True = FTTT1 | FTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'False 'False 'False = TFFF1 | TFFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'False 'False 'True = TFFT1 | TFFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'False 'True 'False = TFTF1 | TFTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'False 'True 'True = TFTT1 | TFTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'True 'False 'False = TTFF1 | TTFF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'True 'False 'True = TTFT1 | TTFT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'True 'True 'False = TTTF1 | TTTF2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)
data instance Foo4 'True 'True 'True 'True = TTTT1 | TTTT2 Int32 deriving (Eq, Show, Read, G.Generic, Aeson.FromJSON, Aeson.ToJSON, Bytes.Serial, Bin.Binary, Cer.Serialize, NFData, Hashable)

--------------------------------------------------------------------------------
-- Arbitrary instances

instance QC.Arbitrary (Foo1 'False) where
  arbitrary = QC.oneof [ pure F1, F2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo1 'True) where
  arbitrary = QC.oneof [ pure T1, T2 <$> QC.arbitrary ]

instance QC.Arbitrary (Foo2 'False 'False) where
  arbitrary = QC.oneof [ pure FF1, FF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo2 'False 'True) where
  arbitrary = QC.oneof [ pure FT1, FT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo2 'True 'False) where
  arbitrary = QC.oneof [ pure TF1, TF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo2 'True 'True) where
  arbitrary = QC.oneof [ pure TT1, TT2 <$> QC.arbitrary ]

instance QC.Arbitrary (Foo3 'False 'False 'False) where
  arbitrary = QC.oneof [ pure FFF1, FFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'False 'False 'True) where
  arbitrary = QC.oneof [ pure FFT1, FFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'False 'True 'False) where
  arbitrary = QC.oneof [ pure FTF1, FTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'False 'True 'True) where
  arbitrary = QC.oneof [ pure FTT1, FTT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'True 'False 'False) where
  arbitrary = QC.oneof [ pure TFF1, TFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'True 'False 'True) where
  arbitrary = QC.oneof [ pure TFT1, TFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'True 'True 'False) where
  arbitrary = QC.oneof [ pure TTF1, TTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo3 'True 'True 'True) where
  arbitrary = QC.oneof [ pure TTT1, TTT2 <$> QC.arbitrary ]

instance QC.Arbitrary (Foo4 'False 'False 'False 'False) where
  arbitrary = QC.oneof [ pure FFFF1, FFFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'False 'False 'True) where
  arbitrary = QC.oneof [ pure FFFT1, FFFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'False 'True 'False) where
  arbitrary = QC.oneof [ pure FFTF1, FFTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'False 'True 'True) where
  arbitrary = QC.oneof [ pure FFTT1, FFTT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'True 'False 'False) where
  arbitrary = QC.oneof [ pure FTFF1, FTFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'True 'False 'True) where
  arbitrary = QC.oneof [ pure FTFT1, FTFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'True 'True 'False) where
  arbitrary = QC.oneof [ pure FTTF1, FTTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'False 'True 'True 'True) where
  arbitrary = QC.oneof [ pure FTTT1, FTTT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'False 'False 'False) where
  arbitrary = QC.oneof [ pure TFFF1, TFFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'False 'False 'True) where
  arbitrary = QC.oneof [ pure TFFT1, TFFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'False 'True 'False) where
  arbitrary = QC.oneof [ pure TFTF1, TFTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'False 'True 'True) where
  arbitrary = QC.oneof [ pure TFTT1, TFTT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'True 'False 'False) where
  arbitrary = QC.oneof [ pure TTFF1, TTFF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'True 'False 'True) where
  arbitrary = QC.oneof [ pure TTFT1, TTFT2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'True 'True 'False) where
  arbitrary = QC.oneof [ pure TTTF1, TTTF2 <$> QC.arbitrary ]
instance QC.Arbitrary (Foo4 'True 'True 'True 'True) where
  arbitrary = QC.oneof [ pure TTTT1, TTTT2 <$> QC.arbitrary ]
