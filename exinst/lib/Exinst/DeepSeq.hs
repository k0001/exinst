{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'NFData' instances for 'Some1', 'Some2', 'Some3' and
-- 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2', 'Dict3'
-- and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.DeepSeq () where

import Control.DeepSeq (NFData(rnf))
import Data.Constraint
import Prelude

import Exinst.Internal
import Exinst.Internal.Sum
import Exinst.Internal.Product

--------------------------------------------------------------------------------

instance forall k1 (f :: k1 -> *).
  ( Dict1 NFData f
  ) => NFData (Some1 f) where
  {-# INLINABLE rnf #-}
  rnf = \(!some1x) ->
    withSome1Sing some1x $ \ !sa1 !(x :: f a1) ->
       case dict1 sa1 :: Dict (NFData (f a1)) of
          Dict -> rnf x `seq` ()

instance forall k2 k1 (f :: k2 -> k1 -> *).
  ( Dict2 NFData f
  ) => NFData (Some2 f) where
  {-# INLINABLE rnf #-}
  rnf = \(!some2x) ->
    withSome2Sing some2x $ \ !sa2 !sa1 !(x :: f a2 a1) ->
       case dict2 sa2 sa1 :: Dict (NFData (f a2 a1)) of
          Dict -> rnf x `seq` ()

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> *).
  ( Dict3 NFData f
  ) => NFData (Some3 f) where
  {-# INLINABLE rnf #-}
  rnf = \(!some3x) ->
    withSome3Sing some3x $ \ !sa3 !sa2 !sa1 !(x :: f a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (NFData (f a3 a2 a1)) of
          Dict -> rnf x `seq` ()

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> *).
  ( Dict4 NFData f
  ) => NFData (Some4 f) where
  {-# INLINABLE rnf #-}
  rnf = \(!some4x) ->
    withSome4Sing some4x $ \ !(sa4) !sa3 !sa2 !sa1 !(x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (NFData (f a4 a3 a2 a1)) of
          Dict -> rnf x `seq` ()

--------------------------------------------------------------------------------

instance (NFData (l a1), NFData (r a1)) => NFData (S1 l r a1)
instance (NFData (l a2 a1), NFData (r a2 a1)) => NFData (S2 l r a2 a1)
instance (NFData (l a3 a2 a1), NFData (r a3 a2 a1)) => NFData (S3 l r a3 a2 a1)
instance (NFData (l a4 a3 a2 a1), NFData (r a4 a3 a2 a1)) => NFData (S4 l r a4 a3 a2 a1)

--------------------------------------------------------------------------------

instance (NFData (l a1), NFData (r a1)) => NFData (P1 l r a1)
instance (NFData (l a2 a1), NFData (r a2 a1)) => NFData (P2 l r a2 a1)
instance (NFData (l a3 a2 a1), NFData (r a3 a2 a1)) => NFData (P3 l r a3 a2 a1)
instance (NFData (l a4 a3 a2 a1), NFData (r a4 a3 a2 a1)) => NFData (P4 l r a4 a3 a2 a1)

