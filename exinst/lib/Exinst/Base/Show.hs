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

module Exinst.Base.Show () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Prelude

import Exinst.Internal hiding (Some1, Some2, Some3, Some4)
import qualified Exinst.Internal as I

--------------------------------------------------------------------------------
-- Show

-- Internal wrappers used to avoid writing the string manipulation in 'Show'.
data Some1'Show r1 x = Some1 r1 x deriving (Show)
data Some2'Show r2 r1 x = Some2 r2 r1 x deriving (Show)
data Some3'Show r3 r2 r1 x = Some3 r3 r2 r1 x deriving (Show)
data Some4'Show r4 r3 r2 r1 x = Some4 r4 r3 r2 r1 x deriving (Show)

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , Show (Demote k1)
    , Dict1 Show f
    ) => Show (I.Some1 f)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some1x -> withSome1Sing some1x $ \sa1 (x :: f a1) ->
       case dict1 sa1 :: Dict (Show (f a1)) of
          Dict -> showsPrec n (Some1 (fromSing sa1) x)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Show (Demote k2)
    , Show (Demote k1)
    , Dict2 Show f
    ) => Show (I.Some2 f)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Show (f a2 a1)) of
          Dict -> showsPrec n (Some2 (fromSing sa2) (fromSing sa1) x)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Show (Demote k3)
    , Show (Demote k2)
    , Show (Demote k1)
    , Dict3 Show f
    ) => Show (I.Some3 f)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Show (f a3 a2 a1)) of
          Dict -> showsPrec n (Some3 (fromSing sa3) (fromSing sa2) (fromSing sa1) x)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Show (Demote k4)
    , Show (Demote k3)
    , Show (Demote k2)
    , Show (Demote k1)
    , Dict4 Show f
    ) => Show (I.Some4 f)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Show (f a4 a3 a2 a1)) of
          Dict -> showsPrec n (Some4 (fromSing sa4) (fromSing sa3)
                                     (fromSing sa2) (fromSing sa1) x)

