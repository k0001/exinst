{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Kind (Type)
import qualified GHC.Generics as G
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import Generic.Random.Generic (genericArbitrary)

import Data.Singletons (SingKind, Sing, DemoteRep, withSomeSing)
import Exinst.Singletons
import Exinst.Instances.Base ()
import Exinst.Instances.QuickCheck ()

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt =
  Tasty.testGroup "main"
  [ tt_id_generic
  ]

--------------------------------------------------------------------------------

data family Foo1 :: Bool -> Type
data instance Foo1 'False = F1 | F2 Int deriving (Eq, Show, G.Generic)
data instance Foo1 'True = T1 | T2 Int deriving (Eq, Show, G.Generic)

data family Foo2 :: Bool -> Bool -> Type
data instance Foo2 'False 'False = FF1 | FF2 Int deriving (Eq, Show, G.Generic)
data instance Foo2 'False 'True = FT1 | FT2 Int deriving (Eq, Show, G.Generic)
data instance Foo2 'True 'False = TF1 | TF2 Int deriving (Eq, Show, G.Generic)
data instance Foo2 'True 'True = TT1 | TT2 Int deriving (Eq, Show, G.Generic)

data family Foo3 :: Bool -> Bool -> Bool -> Type
data instance Foo3 'False 'False 'False = FFF1 | FFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'False 'False 'True = FFT1 | FFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'False 'True 'False = FTF1 | FTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'False 'True 'True = FTT1 | FTT2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'True 'False 'False = TFF1 | TFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'True 'False 'True = TFT1 | TFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'True 'True 'False = TTF1 | TTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo3 'True 'True 'True = TTT1 | TTT2 Int deriving (Eq, Show, G.Generic)

data family Foo4 :: Bool -> Bool -> Bool -> Bool -> Type
data instance Foo4 'False 'False 'False 'False = FFFF1 | FFFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'False 'False 'True = FFFT1 | FFFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'False 'True 'False = FFTF1 | FFTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'False 'True 'True = FFTT1 | FFTT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'True 'False 'False = FTFF1 | FTFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'True 'False 'True = FTFT1 | FTFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'True 'True 'False = FTTF1 | FTTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'False 'True 'True 'True = FTTT1 | FTTT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'False 'False 'False = TFFF1 | TFFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'False 'False 'True = TFFT1 | TFFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'False 'True 'False = TFTF1 | TFTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'False 'True 'True = TFTT1 | TFTT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'True 'False 'False = TTFF1 | TTFF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'True 'False 'True = TTFT1 | TTFT2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'True 'True 'False = TTTF1 | TTTF2 Int deriving (Eq, Show, G.Generic)
data instance Foo4 'True 'True 'True 'True = TTTT1 | TTTT2 Int deriving (Eq, Show, G.Generic)

--------------------------------------------------------------------------------
-- Arbitrary instances

instance QC.Arbitrary (Foo1 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo1 'True) where arbitrary = genericArbitrary

instance QC.Arbitrary (Foo2 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo2 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo2 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo2 'True 'True) where arbitrary = genericArbitrary

instance QC.Arbitrary (Foo3 'False 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'False 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'False 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'False 'True 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'True 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'True 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'True 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo3 'True 'True 'True) where arbitrary = genericArbitrary

instance QC.Arbitrary (Foo4 'False 'False 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'False 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'False 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'False 'True 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'True 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'True 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'True 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'False 'True 'True 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'False 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'False 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'False 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'False 'True 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'True 'False 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'True 'False 'True) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'True 'True 'False) where arbitrary = genericArbitrary
instance QC.Arbitrary (Foo4 'True 'True 'True 'True) where arbitrary = genericArbitrary

--------------------------------------------------------------------------------

tt_id_generic :: Tasty.TestTree
tt_id_generic =
  Tasty.testGroup ("Identity through GHC's Generic")
  [ QC.testProperty "Some1" $
      QC.forAll QC.arbitrary $ \(x :: Some1 Foo1) -> x === G.to (G.from x)
  , QC.testProperty "Some2" $
      QC.forAll QC.arbitrary $ \(x :: Some2 Foo2) -> x === G.to (G.from x)
  , QC.testProperty "Some3" $
      QC.forAll QC.arbitrary $ \(x :: Some3 Foo3) -> x === G.to (G.from x)
  , QC.testProperty "Some4" $
      QC.forAll QC.arbitrary $ \(x :: Some4 Foo4) -> x === G.to (G.from x)
  ]

