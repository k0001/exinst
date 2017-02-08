{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'QC.arbitrary' instances for 'Exinst.Some1', 'Some2',
-- 'Some3' and 'Some4' from "Exinst", provided situable 'Dict1',
-- 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file for more general documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.QuickCheck () where

import Data.Kind (Type)
import qualified Test.QuickCheck as QC
import Data.Singletons (SingKind, Sing, DemoteRep, withSomeSing)
import Exinst

--------------------------------------------------------------------------------

instance
  forall k1 (f :: k1 -> Type).
  ( SingKind k1
  , QC.Arbitrary (DemoteRep k1)
  , Dict1 QC.Arbitrary f
  ) => QC.Arbitrary (Some1 f) where
  arbitrary = do
    da1 <- QC.arbitrary
    withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
      case dict1 sa1 :: Dict (QC.Arbitrary (f a1)) of
        Dict -> Some1 sa1 <$> QC.arbitrary
  shrink = \s1x -> withSome1Sing s1x $ \sa1 (x :: f a1) ->
    case dict1 sa1 :: Dict (QC.Arbitrary (f a1)) of
      Dict -> Some1 sa1 <$> QC.shrink x

instance
  forall k2 k1 (f :: k2 -> k1 -> Type).
  ( SingKind k2
  , SingKind k1
  , QC.Arbitrary (DemoteRep k2)
  , QC.Arbitrary (DemoteRep k1)
  , Dict2 QC.Arbitrary f
  ) => QC.Arbitrary (Some2 f) where
  arbitrary = do
    da2 <- QC.arbitrary
    da1 <- QC.arbitrary
    withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
      withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
        case dict2 sa2 sa1 :: Dict (QC.Arbitrary (f a2 a1)) of
          Dict -> Some2 sa2 sa1 <$> QC.arbitrary
  shrink = \s2x -> withSome2Sing s2x $ \sa2 sa1 (x :: f a2 a1) ->
    case dict2 sa2 sa1 :: Dict (QC.Arbitrary (f a2 a1)) of
      Dict -> Some2 sa2 sa1 <$> QC.shrink x

instance
  forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type).
  ( SingKind k3
  , SingKind k2
  , SingKind k1
  , QC.Arbitrary (DemoteRep k3)
  , QC.Arbitrary (DemoteRep k2)
  , QC.Arbitrary (DemoteRep k1)
  , Dict3 QC.Arbitrary f
  ) => QC.Arbitrary (Some3 f) where
  arbitrary = do
    da3 <- QC.arbitrary
    da2 <- QC.arbitrary
    da1 <- QC.arbitrary
    withSomeSing da3 $ \(sa3 :: Sing (a3 :: k3)) ->
      withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
        withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
          case dict3 sa3 sa2 sa1 :: Dict (QC.Arbitrary (f a3 a2 a1)) of
            Dict -> Some3 sa3 sa2 sa1 <$> QC.arbitrary
  shrink = \s3x -> withSome3Sing s3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
    case dict3 sa3 sa2 sa1 :: Dict (QC.Arbitrary (f a3 a2 a1)) of
      Dict -> Some3 sa3 sa2 sa1 <$> QC.shrink x

instance
  forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type).
  ( SingKind k4
  , SingKind k3
  , SingKind k2
  , SingKind k1
  , QC.Arbitrary (DemoteRep k4)
  , QC.Arbitrary (DemoteRep k3)
  , QC.Arbitrary (DemoteRep k2)
  , QC.Arbitrary (DemoteRep k1)
  , Dict4 QC.Arbitrary f
  ) => QC.Arbitrary (Some4 f) where
  arbitrary = do
    da4 <- QC.arbitrary
    da3 <- QC.arbitrary
    da2 <- QC.arbitrary
    da1 <- QC.arbitrary
    withSomeSing da4 $ \(sa4 :: Sing (a4 :: k4)) ->
      withSomeSing da3 $ \(sa3 :: Sing (a3 :: k3)) ->
        withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
          withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict4 sa4 sa3 sa2 sa1 :: Dict (QC.Arbitrary (f a4 a3 a2 a1)) of
              Dict -> Some4 sa4 sa3 sa2 sa1 <$> QC.arbitrary
  shrink = \s3x -> withSome4Sing s3x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
    case dict4 sa4 sa3 sa2 sa1 :: Dict (QC.Arbitrary (f a4 a3 a2 a1)) of
      Dict -> Some4 sa4 sa3 sa2 sa1 <$> QC.shrink x

