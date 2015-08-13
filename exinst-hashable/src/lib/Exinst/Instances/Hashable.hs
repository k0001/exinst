{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Hashable' instances for 'Some1', 'Some2', 'Some3' and
-- 'Some4' from "Exinst.Singletons", provided situable 'Dict1', 'Dict2', 'Dict3'
-- and 'Dict4' instances are available.
module Exinst.Instances.Hashable () where

import           Data.Hashable (Hashable(hashWithSalt))
import           Data.Constraint
import           Data.Singletons
import           Exinst.Singletons
import           Prelude

--------------------------------------------------------------------------------

-- | Some salt we add to hashes calculated in this module.
salt0 :: Int
salt0 = 6700417

--------------------------------------------------------------------------------

instance forall (f1 :: k1 -> *)
  . ( SingKind ('KProxy :: KProxy k1)
    , Hashable (DemoteRep ('KProxy :: KProxy k1))
    , Dict1 Hashable f1
    ) => Hashable (Some1 f1)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some1 = withSome1Sing some1 $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (Hashable (f1 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f2 :: k2 -> k1 -> *)
  . ( SingKind ('KProxy :: KProxy k2)
    , SingKind ('KProxy :: KProxy k1)
    , Hashable (DemoteRep ('KProxy :: KProxy k2))
    , Hashable (DemoteRep ('KProxy :: KProxy k1))
    , Dict2 Hashable f2
    ) => Hashable (Some2 f2)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some2 = withSome2Sing some2 $ \sa2 sa1 (x :: f2 a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Hashable (f2 a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f3 :: k3 -> k2 -> k1 -> *)
  . ( SingKind ('KProxy :: KProxy k3)
    , SingKind ('KProxy :: KProxy k2)
    , SingKind ('KProxy :: KProxy k1)
    , Hashable (DemoteRep ('KProxy :: KProxy k3))
    , Hashable (DemoteRep ('KProxy :: KProxy k2))
    , Hashable (DemoteRep ('KProxy :: KProxy k1))
    , Dict3 Hashable f3
    ) => Hashable (Some3 f3)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some3 = withSome3Sing some3 $ \sa3 sa2 sa1 (x :: f3 a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Hashable (f3 a3 a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa3
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> *)
  . ( SingKind ('KProxy :: KProxy k4)
    , SingKind ('KProxy :: KProxy k3)
    , SingKind ('KProxy :: KProxy k2)
    , SingKind ('KProxy :: KProxy k1)
    , Hashable (DemoteRep ('KProxy :: KProxy k4))
    , Hashable (DemoteRep ('KProxy :: KProxy k3))
    , Hashable (DemoteRep ('KProxy :: KProxy k2))
    , Hashable (DemoteRep ('KProxy :: KProxy k1))
    , Dict4 Hashable f4
    ) => Hashable (Some4 f4)
  where
    {-# INLINABLE hashWithSalt #-}
    hashWithSalt salt some4 = withSome4Sing some4 $ \sa4 sa3 sa2 sa1 (x :: f4 a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Hashable (f4 a4 a3 a2 a1)) of
          Dict -> salt `hashWithSalt` salt0
                       `hashWithSalt` fromSing sa4
                       `hashWithSalt` fromSing sa3
                       `hashWithSalt` fromSing sa2
                       `hashWithSalt` fromSing sa1
                       `hashWithSalt` x
