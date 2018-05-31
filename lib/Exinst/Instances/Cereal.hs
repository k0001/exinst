{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Cer.Serialize' instances for 'Some1', 'Some2', 'Some3'
-- and 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2',
-- 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Cereal () where

import qualified Data.Serialize as Cer
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude

import Exinst.Internal

--------------------------------------------------------------------------------

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Binary.Binary' instance, provided all of the 'Demote's and the fully
-- applied @f@ instances are compatible as well.
instance forall (f :: k1 -> Type).
  ( SingKind k1
  , Cer.Serialize (Demote k1)
  , Dict1 Cer.Serialize f
  ) => Cer.Serialize (Some1 f) where
  {-# INLINABLE put #-}
  put = \some1x ->
    withSome1Sing some1x $ \sa1 (x :: f a1) ->
      case dict1 sa1 :: Dict (Cer.Serialize (f a1)) of
        Dict -> do
          Cer.put (fromSing sa1)
          Cer.put x
  {-# INLINABLE get #-}
  get = do
    rsa1 <- Cer.get
    withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
      case dict1 sa1 :: Dict (Cer.Serialize (f a1)) of
        Dict -> do
          x :: f a1 <- Cer.get
          pure (Some1 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Binary.Binary' instance, provided all of the 'Demote's and the fully
-- applied @f@ instances are compatible as well.
instance forall (f :: k2 -> k1 -> Type).
  ( SingKind k2
  , SingKind k1
  , Cer.Serialize (Demote k2)
  , Cer.Serialize (Demote k1)
  , Dict2 Cer.Serialize f
  ) => Cer.Serialize (Some2 f) where
  {-# INLINABLE put #-}
  put = \some2x ->
    withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
      case dict2 sa2 sa1 :: Dict (Cer.Serialize (f a2 a1)) of
        Dict -> do
          Cer.put (fromSing sa2, fromSing sa1)
          Cer.put x
  {-# INLINABLE get #-}
  get = do
    (rsa2, rsa1) <- Cer.get
    withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
        case dict2 sa2 sa1 :: Dict (Cer.Serialize (f a2 a1)) of
          Dict -> do
            x :: f a2 a1 <- Cer.get
            pure (Some2 sa2 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Binary.Binary' instance, provided all of the 'Demote's and the fully
-- applied @f@ instances are compatible as well.
instance forall (f :: k3 -> k2 -> k1 -> Type).
  ( SingKind k3
  , SingKind k2
  , SingKind k1
  , Cer.Serialize (Demote k3)
  , Cer.Serialize (Demote k2)
  , Cer.Serialize (Demote k1)
  , Dict3 Cer.Serialize f
  ) => Cer.Serialize (Some3 f) where
  {-# INLINABLE put #-}
  put = \some3x ->
    withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
      case dict3 sa3 sa2 sa1 :: Dict (Cer.Serialize (f a3 a2 a1)) of
        Dict -> do
          Cer.put (fromSing sa3, fromSing sa2, fromSing sa1)
          Cer.put x
  {-# INLINABLE get #-}
  get = do
    (rsa3, rsa2, rsa1) <- Cer.get
    withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
        withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
          case dict3 sa3 sa2 sa1 :: Dict (Cer.Serialize (f a3 a2 a1)) of
            Dict -> do
              x :: f a3 a2 a1 <- Cer.get
              pure (Some3 sa3 sa2 sa1 x)

-- | Compatible with the 'Data.Bytes.Serial.Serial' instance and
-- 'Data.Binary.Binary' instance, provided all of the 'Demote's and the fully
-- applied @f@ instances are compatible as well.
instance forall (f :: k4 -> k3 -> k2 -> k1 -> Type).
  ( SingKind k4
  , SingKind k3
  , SingKind k2
  , SingKind k1
  , Cer.Serialize (Demote k4)
  , Cer.Serialize (Demote k3)
  , Cer.Serialize (Demote k2)
  , Cer.Serialize (Demote k1)
  , Dict4 Cer.Serialize f
  ) => Cer.Serialize (Some4 f) where
  {-# INLINABLE put #-}
  put = \some4x ->
    withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
      case dict4 sa4 sa3 sa2 sa1 :: Dict (Cer.Serialize (f a4 a3 a2 a1)) of
        Dict -> do
          Cer.put (fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1)
          Cer.put x
  {-# INLINABLE get #-}
  get = do
    (rsa4, rsa3, rsa2, rsa1) <- Cer.get
    withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
        withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
          withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict4 sa4 sa3 sa2 sa1 :: Dict (Cer.Serialize (f a4 a3 a2 a1)) of
              Dict -> do
                x :: f a4 a3 a2 a1 <- Cer.get
                pure (Some4 sa4 sa3 sa2 sa1 x)

