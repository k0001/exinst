{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Ae.FromJSON' and 'Ae.ToJSON' instances for 'Some1',
-- 'Some2', 'Some3' and 'Some4' from "Exinst", provided situable
-- 'Dict1', 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Aeson () where

import qualified Data.Aeson as Ae
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Exinst
import Prelude

--------------------------------------------------------------------------------

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , Ae.ToJSON (Demote k1)
    , Dict1 Ae.ToJSON f
    ) => Ae.ToJSON (Some1 f)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some1x -> withSome1Sing some1x $ \sa1 (x :: f a1) ->
       case dict1 sa1 :: Dict (Ae.ToJSON (f a1)) of
          Dict -> Ae.toJSON (fromSing sa1, x)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Ae.ToJSON (Demote k2)
    , Ae.ToJSON (Demote k1)
    , Dict2 Ae.ToJSON f
    ) => Ae.ToJSON (Some2 f)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Ae.ToJSON (f a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa2, fromSing sa1), x)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.ToJSON (Demote k3)
    , Ae.ToJSON (Demote k2)
    , Ae.ToJSON (Demote k1)
    , Dict3 Ae.ToJSON f
    ) => Ae.ToJSON (Some3 f)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Ae.ToJSON (f a3 a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa3, fromSing sa2, fromSing sa1), x)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.ToJSON (Demote k4)
    , Ae.ToJSON (Demote k3)
    , Ae.ToJSON (Demote k2)
    , Ae.ToJSON (Demote k1)
    , Dict4 Ae.ToJSON f
    ) => Ae.ToJSON (Some4 f)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Ae.ToJSON (f a4 a3 a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1), x)

--------------------------------------------------------------------------------

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , Ae.FromJSON (Demote k1)
    , Dict1 Ae.FromJSON f
    ) => Ae.FromJSON (Some1 f)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      (rsa1, v') <- Ae.parseJSON v
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
         case dict1 sa1 :: Dict (Ae.FromJSON (f a1)) of
            Dict -> do
               x :: f a1 <- Ae.parseJSON v'
               pure (Some1 sa1 x)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Ae.FromJSON (Demote k2)
    , Ae.FromJSON (Demote k1)
    , Dict2 Ae.FromJSON f
    ) => Ae.FromJSON (Some2 f)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
         withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict2 sa2 sa1 :: Dict (Ae.FromJSON (f a2 a1)) of
               Dict -> do
                  x :: f a2 a1 <- Ae.parseJSON v'
                  pure (Some2 sa2 sa1 x)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.FromJSON (Demote k3)
    , Ae.FromJSON (Demote k2)
    , Ae.FromJSON (Demote k1)
    , Dict3 Ae.FromJSON f
    ) => Ae.FromJSON (Some3 f)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa3, rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
         withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
            withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
               case dict3 sa3 sa2 sa1 :: Dict (Ae.FromJSON (f a3 a2 a1)) of
                  Dict -> do
                     x :: f a3 a2 a1 <- Ae.parseJSON v'
                     pure (Some3 sa3 sa2 sa1 x)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.FromJSON (Demote k4)
    , Ae.FromJSON (Demote k3)
    , Ae.FromJSON (Demote k2)
    , Ae.FromJSON (Demote k1)
    , Dict4 Ae.FromJSON f
    ) => Ae.FromJSON (Some4 f)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa4, rsa3, rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
         withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
            withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
               withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
                  case dict4 sa4 sa3 sa2 sa1 :: Dict (Ae.FromJSON (f a4 a3 a2 a1)) of
                     Dict -> do
                        x :: f a4 a3 a2 a1 <- Ae.parseJSON v'
                        pure (Some4 sa4 sa3 sa2 sa1 x)

--------------------------------------------------------------------------------

instance (Ae.FromJSON (l a1), Ae.FromJSON (r a1)) => Ae.FromJSON (S1 l r a1)
instance (Ae.FromJSON (l a2 a1), Ae.FromJSON (r a2 a1)) => Ae.FromJSON (S2 l r a2 a1)
instance (Ae.FromJSON (l a3 a2 a1), Ae.FromJSON (r a3 a2 a1)) => Ae.FromJSON (S3 l r a3 a2 a1)
instance (Ae.FromJSON (l a4 a3 a2 a1), Ae.FromJSON (r a4 a3 a2 a1)) => Ae.FromJSON (S4 l r a4 a3 a2 a1)

instance (Ae.ToJSON (l a1), Ae.ToJSON (r a1)) => Ae.ToJSON (S1 l r a1)
instance (Ae.ToJSON (l a2 a1), Ae.ToJSON (r a2 a1)) => Ae.ToJSON (S2 l r a2 a1)
instance (Ae.ToJSON (l a3 a2 a1), Ae.ToJSON (r a3 a2 a1)) => Ae.ToJSON (S3 l r a3 a2 a1)
instance (Ae.ToJSON (l a4 a3 a2 a1), Ae.ToJSON (r a4 a3 a2 a1)) => Ae.ToJSON (S4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------

instance (Ae.FromJSON (l a1), Ae.FromJSON (r a1)) => Ae.FromJSON (P1 l r a1)
instance (Ae.FromJSON (l a2 a1), Ae.FromJSON (r a2 a1)) => Ae.FromJSON (P2 l r a2 a1)
instance (Ae.FromJSON (l a3 a2 a1), Ae.FromJSON (r a3 a2 a1)) => Ae.FromJSON (P3 l r a3 a2 a1)
instance (Ae.FromJSON (l a4 a3 a2 a1), Ae.FromJSON (r a4 a3 a2 a1)) => Ae.FromJSON (P4 l r a4 a3 a2 a1)

instance (Ae.ToJSON (l a1), Ae.ToJSON (r a1)) => Ae.ToJSON (P1 l r a1)
instance (Ae.ToJSON (l a2 a1), Ae.ToJSON (r a2 a1)) => Ae.ToJSON (P2 l r a2 a1)
instance (Ae.ToJSON (l a3 a2 a1), Ae.ToJSON (r a3 a2 a1)) => Ae.ToJSON (P3 l r a3 a2 a1)
instance (Ae.ToJSON (l a4 a3 a2 a1), Ae.ToJSON (r a4 a3 a2 a1)) => Ae.ToJSON (P4 l r a4 a3 a2 a1)


