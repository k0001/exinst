{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'QC.arbitrary' instances for 'Exinst.Some1', 'Some2',
-- 'Some3' and 'Some4' from "Exinst", provided situable 'Dict1',
-- 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file for more general documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst.QuickCheck () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons (SingKind, Sing, Demote, withSomeSing)
import qualified Test.QuickCheck as QC

import Exinst.Internal
import Exinst.Internal.Sum
import Exinst.Internal.Product

--------------------------------------------------------------------------------

instance
  forall k1 (f :: k1 -> Type).
  ( SingKind k1
  , QC.Arbitrary (Demote k1)
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
  , QC.Arbitrary (Demote k2)
  , QC.Arbitrary (Demote k1)
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
  , QC.Arbitrary (Demote k3)
  , QC.Arbitrary (Demote k2)
  , QC.Arbitrary (Demote k1)
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
  , QC.Arbitrary (Demote k4)
  , QC.Arbitrary (Demote k3)
  , QC.Arbitrary (Demote k2)
  , QC.Arbitrary (Demote k1)
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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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
