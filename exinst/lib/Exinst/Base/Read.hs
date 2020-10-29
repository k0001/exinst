{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Exinst.Base.Read () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude
import qualified Text.Read as Read

import Exinst.Internal 

--------------------------------------------------------------------------------
-- Read

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , Read (Demote k1)
    , Dict1 Read f
    ) => Read (Some1 f)
  where
    {-# INLINABLE readPrec #-}
    readPrec = do
      Read.Ident "Some1" <- Read.lexP
      rsa1 <- Read.readPrec
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
         case dict1 sa1 :: Dict (Read (f a1)) of
            Dict -> do
               x :: f a1 <- Read.readPrec
               pure (Some1 sa1 x)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Read (Demote k2)
    , Read (Demote k1)
    , Dict2 Read f
    ) => Read (Some2 f)
  where
    {-# INLINABLE readPrec #-}
    readPrec = do
      Read.Ident "Some2" <- Read.lexP
      rsa2 <- Read.readPrec
      rsa1 <- Read.readPrec
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
         withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict2 sa2 sa1 :: Dict (Read (f a2 a1)) of
               Dict -> do
                  x :: f a2 a1 <- Read.readPrec
                  pure (Some2 sa2 sa1 x)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Read (Demote k3)
    , Read (Demote k2)
    , Read (Demote k1)
    , Dict3 Read f
    ) => Read (Some3 f)
  where
    {-# INLINABLE readPrec #-}
    readPrec = do
      Read.Ident "Some3" <- Read.lexP
      rsa3 <- Read.readPrec
      rsa2 <- Read.readPrec
      rsa1 <- Read.readPrec
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
         withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
            withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
               case dict3 sa3 sa2 sa1 :: Dict (Read (f a3 a2 a1)) of
                  Dict -> do
                     x :: f a3 a2 a1 <- Read.readPrec
                     pure (Some3 sa3 sa2 sa1 x)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Read (Demote k4)
    , Read (Demote k3)
    , Read (Demote k2)
    , Read (Demote k1)
    , Dict4 Read f
    ) => Read (Some4 f)
  where
    {-# INLINABLE readPrec #-}
    readPrec = do
      Read.Ident "Some4" <- Read.lexP
      rsa4 <- Read.readPrec
      rsa3 <- Read.readPrec
      rsa2 <- Read.readPrec
      rsa1 <- Read.readPrec
      withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) ->
         withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) ->
            withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) ->
               withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
                  case dict4 sa4 sa3 sa2 sa1 :: Dict (Read (f a4 a3 a2 a1)) of
                     Dict -> do
                        x :: f a4 a3 a2 a1 <- Read.readPrec
                        pure (Some4 sa4 sa3 sa2 sa1 x)

