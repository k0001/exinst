{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Show', 'Eq' and 'Ord' instances for 'Exinst.Some1',
-- 'Exinst.Some2', 'Exinst.Some3' and 'Exinst.Some4' from "Exinst.Singletons", provided situable
-- 'Dict1', 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file for more general documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Base () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.Decide
import Data.Type.Equality
import Exinst.Singletons
  hiding (Some1(..), Some2(..), Some3(..), Some4(..))
import qualified Exinst.Singletons as Exinst
import Prelude

--------------------------------------------------------------------------------

-- Internal wrappers used to avoid writing the string manipulation in 'Show'
data Some1'Show r1 x = Some1 r1 x deriving (Show)
data Some2'Show r2 r1 x = Some2 r2 r1 x deriving (Show)
data Some3'Show r3 r2 r1 x = Some3 r3 r2 r1 x deriving (Show)
data Some4'Show r4 r3 r2 r1 x = Some4 r4 r3 r2 r1 x deriving (Show)

--------------------------------------------------------------------------------
-- Show

instance forall (f1 :: k1 -> Type)
  . ( SingKind k1
    , Show (DemoteRep k1)
    , Dict1 Show f1
    ) => Show (Exinst.Some1 f1)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some1x -> withSome1Sing some1x $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (Show (f1 a1)) of
          Dict -> showsPrec n (Some1 (fromSing sa1) x)

instance forall (f2 :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Show (DemoteRep k2)
    , Show (DemoteRep k1)
    , Dict2 Show f2
    ) => Show (Exinst.Some2 f2)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f2 a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Show (f2 a2 a1)) of
          Dict -> showsPrec n (Some2 (fromSing sa2) (fromSing sa1) x)

instance forall (f3 :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Show (DemoteRep k3)
    , Show (DemoteRep k2)
    , Show (DemoteRep k1)
    , Dict3 Show f3
    ) => Show (Exinst.Some3 f3)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f3 a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Show (f3 a3 a2 a1)) of
          Dict -> showsPrec n (Some3 (fromSing sa3) (fromSing sa2) (fromSing sa1) x)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Show (DemoteRep k4)
    , Show (DemoteRep k3)
    , Show (DemoteRep k2)
    , Show (DemoteRep k1)
    , Dict4 Show f4
    ) => Show (Exinst.Some4 f4)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f4 a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Show (f4 a4 a3 a2 a1)) of
          Dict -> showsPrec n (Some4 (fromSing sa4) (fromSing sa3)
                                     (fromSing sa2) (fromSing sa1) x)

--------------------------------------------------------------------------------
-- Read

--------------------------------------------------------------------------------
-- Eq

instance forall (f1 :: k1 -> Type)
  . ( SDecide k1
    , Dict1 Eq f1
    ) => Eq (Exinst.Some1 f1)
  where
    {-# INLINABLE (==) #-}
    (==) = \som1x som1y ->
       withSome1Sing som1x $ \sa1x (x :: f1 a1x) ->
          withSome1Sing som1y $ \sa1y (y :: f1 a1y) ->
             maybe False id $ do
                Refl <- testEquality sa1x sa1y
                case dict1 sa1x :: Dict (Eq (f1 a1x)) of
                   Dict -> Just (x == y)

instance forall (f2 :: k2 -> k1 -> Type)
  . ( SDecide k2
    , SDecide k1
    , Dict2 Eq f2
    ) => Eq (Exinst.Some2 f2)
  where
    {-# INLINABLE (==) #-}
    (==) = \som2x som2y ->
       withSome2Sing som2x $ \sa2x sa1x (x :: f2 a2x a1x) ->
          withSome2Sing som2y $ \sa2y sa1y (y :: f2 a2y a1y) ->
             maybe False id $ do
                Refl <- testEquality sa2x sa2y
                Refl <- testEquality sa1x sa1y
                case dict2 sa2x sa1x :: Dict (Eq (f2 a2x a1x)) of
                   Dict -> Just (x == y)

instance forall (f3 :: k3 -> k2 -> k1 -> Type)
  . ( SDecide k3
    , SDecide k2
    , SDecide k1
    , Dict3 Eq f3
    ) => Eq (Exinst.Some3 f3)
  where
    {-# INLINABLE (==) #-}
    (==) = \som3x som3y ->
       withSome3Sing som3x $ \sa3x sa2x sa1x (x :: f3 a3x a2x a1x) ->
          withSome3Sing som3y $ \sa3y sa2y sa1y (y :: f3 a3y a2y a1y) ->
             maybe False id $ do
                Refl <- testEquality sa3x sa3y
                Refl <- testEquality sa2x sa2y
                Refl <- testEquality sa1x sa1y
                case dict3 sa3x sa2x sa1x :: Dict (Eq (f3 a3x a2x a1x)) of
                   Dict -> Just (x == y)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SDecide k4
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Dict4 Eq f4
    ) => Eq (Exinst.Some4 f4)
  where
    {-# INLINABLE (==) #-}
    (==) = \som4x som4y ->
       withSome4Sing som4x $ \sa4x sa3x sa2x sa1x (x :: f4 a4x a3x a2x a1x) ->
          withSome4Sing som4y $ \sa4y sa3y sa2y sa1y (y :: f4 a4y a3y a2y a1y) ->
             maybe False id $ do
                Refl <- testEquality sa4x sa4y
                Refl <- testEquality sa3x sa3y
                Refl <- testEquality sa2x sa2y
                Refl <- testEquality sa1x sa1y
                case dict4 sa4x sa3x sa2x sa1x :: Dict (Eq (f4 a4x a3x a2x a1x)) of
                   Dict -> Just (x == y)

--------------------------------------------------------------------------------
-- Ord

instance forall (f1 :: k1 -> Type)
  . ( SingKind k1
    , SDecide k1
    , Ord (DemoteRep k1)
    , Dict1 Ord f1
    , Eq (Exinst.Some1 f1)
    ) => Ord (Exinst.Some1 f1)
  where
    {-# INLINABLE compare #-}
    compare = \som1x som1y ->
       withSome1Sing som1x $ \sa1x (x :: f1 a1x) ->
          withSome1Sing som1y $ \sa1y (y :: f1 a1y) ->
             let termCompare = compare (fromSing sa1x) (fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- testEquality sa1x sa1y
                  case dict1 sa1x :: Dict (Ord (f1 a1x)) of
                     Dict -> Just (compare x y)

instance forall (f2 :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , SDecide k2
    , SDecide k1
    , Ord (DemoteRep k2)
    , Ord (DemoteRep k1)
    , Dict2 Ord f2
    , Eq (Exinst.Some2 f2)
    ) => Ord (Exinst.Some2 f2)
  where
    {-# INLINABLE compare #-}
    compare = \som2x som2y ->
       withSome2Sing som2x $ \sa2x sa1x (x :: f2 a2x a1x) ->
          withSome2Sing som2y $ \sa2y sa1y (y :: f2 a2y a1y) ->
             let termCompare = compare (fromSing sa2x, fromSing sa1x)
                                       (fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                   Refl <- testEquality sa2x sa2y
                   Refl <- testEquality sa1x sa1y
                   case dict2 sa2x sa1x :: Dict (Ord (f2 a2x a1x)) of
                      Dict -> Just (compare x y)

instance forall (f3 :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Ord (DemoteRep k3)
    , Ord (DemoteRep k2)
    , Ord (DemoteRep k1)
    , Dict3 Ord f3
    , Eq (Exinst.Some3 f3)
    ) => Ord (Exinst.Some3 f3)
  where
    {-# INLINABLE compare #-}
    compare = \som3x som3y ->
       withSome3Sing som3x $ \sa3x sa2x sa1x (x :: f3 a3x a2x a1x) ->
          withSome3Sing som3y $ \sa3y sa2y sa1y (y :: f3 a3y a2y a1y) ->
             let termCompare = compare
                   (fromSing sa3x, fromSing sa2x, fromSing sa1x)
                   (fromSing sa3y, fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- testEquality sa3x sa3y
                  Refl <- testEquality sa2x sa2y
                  Refl <- testEquality sa1x sa1y
                  case dict3 sa3x sa2x sa1x :: Dict (Ord (f3 a3x a2x a1x)) of
                     Dict -> Just (compare x y)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , SDecide k4
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Ord (DemoteRep k4)
    , Ord (DemoteRep k3)
    , Ord (DemoteRep k2)
    , Ord (DemoteRep k1)
    , Dict4 Ord f4
    , Eq (Exinst.Some4 f4)
    ) => Ord (Exinst.Some4 f4)
  where
    {-# INLINABLE compare #-}
    compare = \som4x som4y ->
       withSome4Sing som4x $ \sa4x sa3x sa2x sa1x (x :: f4 a4x a3x a2x a1x) ->
          withSome4Sing som4y $ \sa4y sa3y sa2y sa1y (y :: f4 a4y a3y a2y a1y) ->
             let termCompare = compare
                   (fromSing sa4x, fromSing sa3x, fromSing sa2x, fromSing sa1x)
                   (fromSing sa4y, fromSing sa3y, fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- testEquality sa4x sa4y
                  Refl <- testEquality sa3x sa3y
                  Refl <- testEquality sa2x sa2y
                  Refl <- testEquality sa1x sa1y
                  case dict4 sa4x sa3x sa2x sa1x :: Dict (Ord (f4 a4x a3x a2x a1x)) of
                     Dict -> Just (compare x y)
