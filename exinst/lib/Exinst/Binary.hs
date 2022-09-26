{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Bin.Binary' instances for 'Some1', 'Some2', 'Some3'
-- and 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2',
-- 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Binary () where

import qualified Data.Binary as Bin
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude

import Exinst.Internal
import Exinst.Internal.Sum
import Exinst.Internal.Product

--------------------------------------------------------------------------------

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall k1 (f :: k1 -> Type).
  ( SingKind k1
  , Bin.Binary (Demote k1)
  , Dict1 Bin.Binary f
  ) => Bin.Binary (Some1 f) where
  {-# INLINABLE put #-}
  put = \some1x ->
    withSome1Sing some1x $ \sa1 (x :: f a1) ->
      case dict1 sa1 :: Dict (Bin.Binary (f a1)) of
        Dict -> do
          Bin.put (fromSing sa1)
          Bin.put x
  {-# INLINABLE get #-}
  get = do
    rsa1 <- Bin.get
    withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
      case dict1 sa1 :: Dict (Bin.Binary (f a1)) of
        Dict -> do
          x :: f a1 <- Bin.get
          pure (Some1 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall k2 k1 (f :: k2 -> k1 -> Type).
  ( SingKind k2
  , SingKind k1
  , Bin.Binary (Demote k2)
  , Bin.Binary (Demote k1)
  , Dict2 Bin.Binary f
  ) => Bin.Binary (Some2 f) where
  {-# INLINABLE put #-}
  put = \some2x ->
    withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
      case dict2 sa2 sa1 :: Dict (Bin.Binary (f a2 a1)) of
        Dict -> do
          Bin.put (fromSing sa2, fromSing sa1)
          Bin.put x
  {-# INLINABLE get #-}
  get = do
    (rsa2, rsa1) <- Bin.get
    withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
        case dict2 sa2 sa1 :: Dict (Bin.Binary (f a2 a1)) of
          Dict -> do
            x :: f a2 a1 <- Bin.get
            pure (Some2 sa2 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type).
  ( SingKind k3
  , SingKind k2
  , SingKind k1
  , Bin.Binary (Demote k3)
  , Bin.Binary (Demote k2)
  , Bin.Binary (Demote k1)
  , Dict3 Bin.Binary f
  ) => Bin.Binary (Some3 f) where
  {-# INLINABLE put #-}
  put = \some3x ->
    withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
      case dict3 sa3 sa2 sa1 :: Dict (Bin.Binary (f a3 a2 a1)) of
        Dict -> do
          Bin.put (fromSing sa3, fromSing sa2, fromSing sa1)
          Bin.put x
  {-# INLINABLE get #-}
  get = do
    (rsa3, rsa2, rsa1) <- Bin.get
    withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
        withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
          case dict3 sa3 sa2 sa1 :: Dict (Bin.Binary (f a3 a2 a1)) of
            Dict -> do
              x :: f a3 a2 a1 <- Bin.get
              pure (Some3 sa3 sa2 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type).
  ( SingKind k4
  , SingKind k3
  , SingKind k2
  , SingKind k1
  , Bin.Binary (Demote k4)
  , Bin.Binary (Demote k3)
  , Bin.Binary (Demote k2)
  , Bin.Binary (Demote k1)
  , Dict4 Bin.Binary f
  ) => Bin.Binary (Some4 f) where
  {-# INLINABLE put #-}
  put = \some4x ->
    withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
      case dict4 sa4 sa3 sa2 sa1 :: Dict (Bin.Binary (f a4 a3 a2 a1)) of
        Dict -> do
          Bin.put (fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1)
          Bin.put x
  {-# INLINABLE get #-}
  get = do
    (rsa4, rsa3, rsa2, rsa1) <- Bin.get
    withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
        withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
          withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict4 sa4 sa3 sa2 sa1 :: Dict (Bin.Binary (f a4 a3 a2 a1)) of
              Dict -> do
                x :: f a4 a3 a2 a1 <- Bin.get
                pure (Some4 sa4 sa3 sa2 sa1 x)

--------------------------------------------------------------------------------

instance (Bin.Binary (l a1), Bin.Binary (r a1)) => Bin.Binary (S1 l r a1)
instance (Bin.Binary (l a2 a1), Bin.Binary (r a2 a1)) => Bin.Binary (S2 l r a2 a1)
instance (Bin.Binary (l a3 a2 a1), Bin.Binary (r a3 a2 a1)) => Bin.Binary (S3 l r a3 a2 a1)
instance (Bin.Binary (l a4 a3 a2 a1), Bin.Binary (r a4 a3 a2 a1)) => Bin.Binary (S4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------

instance (Bin.Binary (l a1), Bin.Binary (r a1)) => Bin.Binary (P1 l r a1)
instance (Bin.Binary (l a2 a1), Bin.Binary (r a2 a1)) => Bin.Binary (P2 l r a2 a1)
instance (Bin.Binary (l a3 a2 a1), Bin.Binary (r a3 a2 a1)) => Bin.Binary (P3 l r a3 a2 a1)
instance (Bin.Binary (l a4 a3 a2 a1), Bin.Binary (r a4 a3 a2 a1)) => Bin.Binary (P4 l r a4 a3 a2 a1)
