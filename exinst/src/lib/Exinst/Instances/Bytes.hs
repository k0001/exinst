{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'By.Serial' instances for 'Some1', 'Some2', 'Some3'
-- and 'Some4' from "Exinst", provided situable 'Dict1', 'Dict2',
-- 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Bytes () where

import qualified Data.Bytes.Serial as By
import           Data.Constraint
import           Data.Singletons
import           Exinst
import           Prelude

--------------------------------------------------------------------------------

instance forall (f1 :: k1 -> *)
  . ( SingKind k1
    , By.Serial (DemoteRep k1)
    , Dict1 By.Serial f1
    ) => By.Serial (Some1 f1)
  where
    {-# INLINABLE serialize #-}
    serialize = \some1x -> withSome1Sing some1x $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (By.Serial (f1 a1)) of
          Dict -> do By.serialize (fromSing sa1)
                     By.serialize x
    {-# INLINABLE deserialize #-}
    deserialize = do
      rsa1 <- By.deserialize
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
         case dict1 sa1 :: Dict (By.Serial (f1 a1)) of
            Dict -> do x :: f1 a1 <- By.deserialize
                       return (some1 x)

instance forall (f2 :: k2 -> k1 -> *)
  . ( SingKind k2
    , SingKind k1
    , By.Serial (DemoteRep k2)
    , By.Serial (DemoteRep k1)
    , Dict2 By.Serial f2
    ) => By.Serial (Some2 f2)
  where
    {-# INLINABLE serialize #-}
    serialize = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f2 a2 a1) ->
       case dict2 sa2 sa1 :: Dict (By.Serial (f2 a2 a1)) of
          Dict -> do By.serialize (fromSing sa2, fromSing sa1)
                     By.serialize x
    {-# INLINABLE deserialize #-}
    deserialize = do
      (rsa2, rsa1) <- By.deserialize
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
         withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
            case dict2 sa2 sa1 :: Dict (By.Serial (f2 a2 a1)) of
               Dict -> do x :: f2 a2 a1 <- By.deserialize
                          return (some2 x)

instance forall (f3 :: k3 -> k2 -> k1 -> *)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , By.Serial (DemoteRep k3)
    , By.Serial (DemoteRep k2)
    , By.Serial (DemoteRep k1)
    , Dict3 By.Serial f3
    ) => By.Serial (Some3 f3)
  where
    {-# INLINABLE serialize #-}
    serialize = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f3 a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (By.Serial (f3 a3 a2 a1)) of
          Dict -> do By.serialize (fromSing sa3, fromSing sa2, fromSing sa1)
                     By.serialize x
    {-# INLINABLE deserialize #-}
    deserialize = do
      (rsa3, rsa2, rsa1) <- By.deserialize
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) -> withSingI sa3 $
         withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
            withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
               case dict3 sa3 sa2 sa1 :: Dict (By.Serial (f3 a3 a2 a1)) of
                  Dict -> do x :: f3 a3 a2 a1 <- By.deserialize
                             return (some3 x)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> *)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , By.Serial (DemoteRep k4)
    , By.Serial (DemoteRep k3)
    , By.Serial (DemoteRep k2)
    , By.Serial (DemoteRep k1)
    , Dict4 By.Serial f4
    ) => By.Serial (Some4 f4)
  where
    {-# INLINABLE serialize #-}
    serialize = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f4 a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (By.Serial (f4 a4 a3 a2 a1)) of
          Dict -> do By.serialize (fromSing sa4, fromSing sa3,
                                   fromSing sa2, fromSing sa1)
                     By.serialize x
    {-# INLINABLE deserialize #-}
    deserialize = do
      (rsa4, rsa3, rsa2, rsa1) <- By.deserialize
      withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) -> withSingI sa4 $
         withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) -> withSingI sa3 $
            withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
               withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
                  case dict4 sa4 sa3 sa2 sa1 :: Dict (By.Serial (f4 a4 a3 a2 a1)) of
                     Dict -> do x :: f4 a4 a3 a2 a1 <- By.deserialize
                                return (some4 x)
