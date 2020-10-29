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

module Exinst.Base.Eq () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons.Decide
import Prelude

import Exinst.Internal 

--------------------------------------------------------------------------------
-- Eq

instance forall k1 (f :: k1 -> Type).
  ( SDecide k1
  , Dict1 Eq f
  ) => Eq (Some1 f)
  where
  {-# INLINABLE (==) #-}
  (==) = \som1x som1y ->
     withSome1Sing som1x $ \sa1x (x :: f a1x) ->
        withSome1Sing som1y $ \sa1y (y :: f a1y) ->
           maybe False id $ do
              Refl <- decideEquality sa1x sa1y
              case dict1 sa1x :: Dict (Eq (f a1x)) of
                 Dict -> Just (x == y)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SDecide k2
    , SDecide k1
    , Dict2 Eq f
    ) => Eq (Some2 f)
  where
    {-# INLINABLE (==) #-}
    (==) = \som2x som2y ->
       withSome2Sing som2x $ \sa2x sa1x (x :: f a2x a1x) ->
          withSome2Sing som2y $ \sa2y sa1y (y :: f a2y a1y) ->
             maybe False id $ do
                Refl <- decideEquality sa2x sa2y
                Refl <- decideEquality sa1x sa1y
                case dict2 sa2x sa1x :: Dict (Eq (f a2x a1x)) of
                   Dict -> Just (x == y)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SDecide k3
    , SDecide k2
    , SDecide k1
    , Dict3 Eq f
    ) => Eq (Some3 f)
  where
    {-# INLINABLE (==) #-}
    (==) = \som3x som3y ->
       withSome3Sing som3x $ \sa3x sa2x sa1x (x :: f a3x a2x a1x) ->
          withSome3Sing som3y $ \sa3y sa2y sa1y (y :: f a3y a2y a1y) ->
             maybe False id $ do
                Refl <- decideEquality sa3x sa3y
                Refl <- decideEquality sa2x sa2y
                Refl <- decideEquality sa1x sa1y
                case dict3 sa3x sa2x sa1x :: Dict (Eq (f a3x a2x a1x)) of
                   Dict -> Just (x == y)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SDecide k4
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Dict4 Eq f
    ) => Eq (Some4 f)
  where
    {-# INLINABLE (==) #-}
    (==) = \som4x som4y ->
       withSome4Sing som4x $ \sa4x sa3x sa2x sa1x (x :: f a4x a3x a2x a1x) ->
          withSome4Sing som4y $ \sa4y sa3y sa2y sa1y (y :: f a4y a3y a2y a1y) ->
             maybe False id $ do
                Refl <- decideEquality sa4x sa4y
                Refl <- decideEquality sa3x sa3y
                Refl <- decideEquality sa2x sa2y
                Refl <- decideEquality sa1x sa1y
                case dict4 sa4x sa3x sa2x sa1x :: Dict (Eq (f a4x a3x a2x a1x)) of
                   Dict -> Just (x == y)
