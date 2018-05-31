{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'By.Serial' instances for 'Some1', 'Some2', 'Some3'
-- and 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2',
-- 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Bytes () where

import qualified Data.Bytes.Serial as By
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude

import Exinst.Internal

--------------------------------------------------------------------------------

-- | Compatible with the 'Data.Binary.Binary' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall (f :: k1 -> Type).
  ( SingKind k1
  , By.Serial (Demote k1)
  , Dict1 By.Serial f
  ) => By.Serial (Some1 f) where
  {-# INLINABLE serialize #-}
  serialize = \some1x ->
    withSome1Sing some1x $ \sa1 (x :: f a1) ->
      case dict1 sa1 :: Dict (By.Serial (f a1)) of
        Dict -> do
          By.serialize (fromSing sa1)
          By.serialize x
  {-# INLINABLE deserialize #-}
  deserialize = do
    rsa1 <- By.deserialize
    withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
      case dict1 sa1 :: Dict (By.Serial (f a1)) of
        Dict -> do
          x :: f a1 <- By.deserialize
          pure (Some1 sa1 x)

-- | Compatible with the 'Data.Binary.Binary' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall (f :: k2 -> k1 -> Type).
  ( SingKind k2
  , SingKind k1
  , By.Serial (Demote k2)
  , By.Serial (Demote k1)
  , Dict2 By.Serial f
  ) => By.Serial (Some2 f) where
  {-# INLINABLE serialize #-}
  serialize = \some2x ->
    withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
      case dict2 sa2 sa1 :: Dict (By.Serial (f a2 a1)) of
        Dict -> do
          By.serialize (fromSing sa2, fromSing sa1)
          By.serialize x
  {-# INLINABLE deserialize #-}
  deserialize = do
    (rsa2, rsa1) <- By.deserialize
    withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
        case dict2 sa2 sa1 :: Dict (By.Serial (f a2 a1)) of
          Dict -> do
            x :: f a2 a1 <- By.deserialize
            pure (Some2 sa2 sa1 x)

-- | Compatible with the 'Data.Binary.Binary' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall (f :: k3 -> k2 -> k1 -> Type).
  ( SingKind k3
  , SingKind k2
  , SingKind k1
  , By.Serial (Demote k3)
  , By.Serial (Demote k2)
  , By.Serial (Demote k1)
  , Dict3 By.Serial f
  ) => By.Serial (Some3 f) where
  {-# INLINABLE serialize #-}
  serialize = \some3x ->
    withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
      case dict3 sa3 sa2 sa1 :: Dict (By.Serial (f a3 a2 a1)) of
        Dict -> do
          By.serialize (fromSing sa3, fromSing sa2, fromSing sa1)
          By.serialize x
  {-# INLINABLE deserialize #-}
  deserialize = do
    (rsa3, rsa2, rsa1) <- By.deserialize
    withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
        withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
           case dict3 sa3 sa2 sa1 :: Dict (By.Serial (f a3 a2 a1)) of
             Dict -> do
               x :: f a3 a2 a1 <- By.deserialize
               pure (Some3 sa3 sa2 sa1 x)

-- | Compatible with the 'Data.Binary.Binary' instance and
-- 'Data.Serialize.Serialize' instance, provided all of the 'Demote's and the
-- fully applied @f@ instances are compatible as well.
instance forall (f :: k4 -> k3 -> k2 -> k1 -> Type).
  ( SingKind k4
  , SingKind k3
  , SingKind k2
  , SingKind k1
  , By.Serial (Demote k4)
  , By.Serial (Demote k3)
  , By.Serial (Demote k2)
  , By.Serial (Demote k1)
  , Dict4 By.Serial f
  ) => By.Serial (Some4 f) where
  {-# INLINABLE serialize #-}
  serialize = \some4x ->
    withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
      case dict4 sa4 sa3 sa2 sa1 :: Dict (By.Serial (f a4 a3 a2 a1)) of
        Dict -> do
          By.serialize (fromSing sa4, fromSing sa3,
                        fromSing sa2, fromSing sa1)
          By.serialize x
  {-# INLINABLE deserialize #-}
  deserialize = do
    (rsa4, rsa3, rsa2, rsa1) <- By.deserialize
    withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
        withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
          withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict4 sa4 sa3 sa2 sa1 :: Dict (By.Serial (f a4 a3 a2 a1)) of
              Dict -> do
                x :: f a4 a3 a2 a1 <- By.deserialize
                pure (Some4 sa4 sa3 sa2 sa1 x)
