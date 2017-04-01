{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Hashable' instances for 'Some1', 'Some2', 'Some3' and
-- 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2', 'Dict3'
-- and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Hashable () where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Constraint
import Data.Singletons
import Prelude

import Exinst.Internal

--------------------------------------------------------------------------------

-- | Some salt we add to hashes calculated in this module.
salt0 :: Int
salt0 = 6700417

--------------------------------------------------------------------------------

instance forall (f :: k1 -> *)
  . ( SingKind k1
    , Hashable (DemoteRep k1)
    , Dict1 Hashable f
    ) => Hashable (Some1 f)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some1x = withSome1Sing some1x $ \sa1 (x :: f a1) ->
       case dict1 sa1 :: Dict (Hashable (f a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f :: k2 -> k1 -> *)
  . ( SingKind k2
    , SingKind k1
    , Hashable (DemoteRep k2)
    , Hashable (DemoteRep k1)
    , Dict2 Hashable f
    ) => Hashable (Some2 f)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some2x = withSome2Sing some2x $ \sa2 sa1 (x :: f a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Hashable (f a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f :: k3 -> k2 -> k1 -> *)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Hashable (DemoteRep k3)
    , Hashable (DemoteRep k2)
    , Hashable (DemoteRep k1)
    , Dict3 Hashable f
    ) => Hashable (Some3 f)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some3x = withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Hashable (f a3 a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa3
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f :: k4 -> k3 -> k2 -> k1 -> *)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Hashable (DemoteRep k4)
    , Hashable (DemoteRep k3)
    , Hashable (DemoteRep k2)
    , Hashable (DemoteRep k1)
    , Dict4 Hashable f
    ) => Hashable (Some4 f)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some4x = withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Hashable (f a4 a3 a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa4
                       `hashWithSalt` fromSing sa3
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x
