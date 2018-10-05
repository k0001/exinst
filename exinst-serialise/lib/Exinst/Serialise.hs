{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Serialise' instances (which provide
-- binary serialisation via the CBOR format) for 'Some1',
-- 'Some2', 'Some3' and 'Some4' from "Exinst", provided situable
-- 'Dict1', 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Serialise () where

import Codec.Serialise
import Codec.Serialise.Decoding (decodeListLenOf)
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude

import Exinst

--------------------------------------------------------------------------------

instance forall (f :: k1 -> Type)
  . ( SingKind k1
    , Serialise (Demote k1)
    , Dict1 Serialise f
    ) => Serialise (Some1 f)
  where
    {-# INLINABLE encode #-}
    encode = \some1x -> withSome1Sing some1x $ \sa1 (x :: f a1) ->
       case dict1 sa1 :: Dict (Serialise (f a1)) of
          Dict -> encode (fromSing sa1, x)
    {-# INLINABLE decode #-}
    decode = do
      decodeListLenOf 2
      rsa1 <- decode
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
         case dict1 sa1 :: Dict (Serialise (f a1)) of
            Dict -> do
               x :: f a1 <- decode
               pure (Some1 sa1 x)

instance forall (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Serialise (Demote k2)
    , Serialise (Demote k1)
    , Dict2 Serialise f
    ) => Serialise (Some2 f)
  where
    {-# INLINABLE encode #-}
    encode = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Serialise (f a2 a1)) of
          Dict -> encode (fromSing sa2, fromSing sa1, x)


    {-# INLINABLE decode #-}
    decode = do
      decodeListLenOf 3
      rsa2 <- decode; rsa1 <- decode
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
         withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict2 sa2 sa1 :: Dict (Serialise (f a2 a1)) of
               Dict -> do
                  x :: f a2 a1 <- decode
                  pure (Some2 sa2 sa1 x)

instance forall (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Serialise (Demote k3)
    , Serialise (Demote k2)
    , Serialise (Demote k1)
    , Dict3 Serialise f
    ) => Serialise (Some3 f)
  where
    {-# INLINABLE encode #-}
    encode = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Serialise (f a3 a2 a1)) of
          Dict -> encode (fromSing sa3, fromSing sa2, fromSing sa1, x)
    {-# INLINABLE decode #-}
    decode = do
      decodeListLenOf 4
      rsa3 <- decode; rsa2 <- decode; rsa1 <- decode
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
         withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
            withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
               case dict3 sa3 sa2 sa1 :: Dict (Serialise (f a3 a2 a1)) of
                  Dict -> do
                     x :: f a3 a2 a1 <- decode
                     pure (Some3 sa3 sa2 sa1 x)

instance forall (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Serialise (Demote k4)
    , Serialise (Demote k3)
    , Serialise (Demote k2)
    , Serialise (Demote k1)
    , Dict4 Serialise f
    ) => Serialise (Some4 f)
  where
    {-# INLINABLE encode #-}
    encode = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Serialise (f a4 a3 a2 a1)) of
          Dict -> encode (fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1, x)
    {-# INLINABLE decode #-}
    decode = do
      decodeListLenOf 5
      rsa4 <- decode; rsa3 <- decode; rsa2 <- decode; rsa1 <- decode;
      withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
         withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
            withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
               withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
                  case dict4 sa4 sa3 sa2 sa1 :: Dict (Serialise (f a4 a3 a2 a1)) of
                     Dict -> do
                        x :: f a4 a3 a2 a1 <- decode
                        pure (Some4 sa4 sa3 sa2 sa1 x)

--------------------------------------------------------------------------------

instance (Serialise (l a1), Serialise (r a1)) => Serialise (S1 l r a1)
instance (Serialise (l a2 a1), Serialise (r a2 a1)) => Serialise (S2 l r a2 a1)
instance (Serialise (l a3 a2 a1), Serialise (r a3 a2 a1)) => Serialise (S3 l r a3 a2 a1)
instance (Serialise (l a4 a3 a2 a1), Serialise (r a4 a3 a2 a1)) => Serialise (S4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------

instance (Serialise (l a1), Serialise (r a1)) => Serialise (P1 l r a1)
instance (Serialise (l a2 a1), Serialise (r a2 a1)) => Serialise (P2 l r a2 a1)
instance (Serialise (l a3 a2 a1), Serialise (r a3 a2 a1)) => Serialise (P3 l r a3 a2 a1)
instance (Serialise (l a4 a3 a2 a1), Serialise (r a4 a3 a2 a1)) => Serialise (P4 l r a4 a3 a2 a1)
