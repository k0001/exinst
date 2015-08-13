{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'NFData' instances for 'Some1', 'Some2', 'Some3' and
-- 'Some4' from "Exinst.Singletons", provided situable 'Dict1', 'Dict2', 'Dict3'
-- and 'Dict4' instances are available.
module Exinst.Instances.DeepSeq () where

import           Control.DeepSeq (NFData(rnf))
import           Data.Constraint
import           Exinst.Singletons
import           Prelude

--------------------------------------------------------------------------------

instance forall (f1 :: k1 -> *)
  . ( Dict1 NFData f1
    ) => NFData (Some1 f1)
  where
    {-# INLINABLE rnf #-}
    rnf !some1 = withSome1Sing some1 $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (NFData (f1 a1)) of
          Dict -> rnf x `seq` ()

instance forall (f2 :: k2 -> k1 -> *)
  . ( Dict2 NFData f2
    ) => NFData (Some2 f2)
  where
    {-# INLINABLE rnf #-}
    rnf !some2 = withSome2Sing some2 $ \sa2 sa1 (x :: f2 a2 a1) ->
       case dict2 sa2 sa1 :: Dict (NFData (f2 a2 a1)) of
          Dict -> rnf x `seq` ()

instance forall (f3 :: k3 -> k2 -> k1 -> *)
  . ( Dict3 NFData f3
    ) => NFData (Some3 f3)
  where
    {-# INLINABLE rnf #-}
    rnf !some3 = withSome3Sing some3 $ \sa3 sa2 sa1 (x :: f3 a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (NFData (f3 a3 a2 a1)) of
          Dict -> rnf x `seq` ()

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> *)
  . ( Dict4 NFData f4
    ) => NFData (Some4 f4)
  where
    {-# INLINABLE rnf #-}
    rnf !some4 = withSome4Sing some4 $ \sa4 sa3 sa2 sa1 (x :: f4 a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (NFData (f4 a4 a3 a2 a1)) of
          Dict -> rnf x `seq` ()
