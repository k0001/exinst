{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Ae.FromJSON' and 'Ae.ToJSON' instances for 'Some1',
-- 'Some2', 'Some3' and 'Some4' from "Exinst.Singletons", provided situable
-- 'Dict1', 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file in the @exinst@ package for more general documentation:
-- https://hackage.haskell.org/package/exinst#readme
module Exinst.Instances.Aeson () where

import qualified Data.Aeson as Ae
import           Data.Constraint
import           Data.Singletons
import           Exinst.Singletons
import           Prelude

--------------------------------------------------------------------------------

instance forall (f1 :: k1 -> *)
  . ( SingKind k1
    , Ae.ToJSON (DemoteRep k1)
    , Dict1 Ae.ToJSON f1
    ) => Ae.ToJSON (Some1 f1)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some1x -> withSome1Sing some1x $ \sa1 (x :: f1 a1) ->
       case dict1 sa1 :: Dict (Ae.ToJSON (f1 a1)) of
          Dict -> Ae.toJSON (fromSing sa1, x)

instance forall (f2 :: k2 -> k1 -> *)
  . ( SingKind k2
    , SingKind k1
    , Ae.ToJSON (DemoteRep k2)
    , Ae.ToJSON (DemoteRep k1)
    , Dict2 Ae.ToJSON f2
    ) => Ae.ToJSON (Some2 f2)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some2x -> withSome2Sing some2x $ \sa2 sa1 (x :: f2 a2 a1) ->
       case dict2 sa2 sa1 :: Dict (Ae.ToJSON (f2 a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa2, fromSing sa1), x)

instance forall (f3 :: k3 -> k2 -> k1 -> *)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.ToJSON (DemoteRep k3)
    , Ae.ToJSON (DemoteRep k2)
    , Ae.ToJSON (DemoteRep k1)
    , Dict3 Ae.ToJSON f3
    ) => Ae.ToJSON (Some3 f3)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some3x -> withSome3Sing some3x $ \sa3 sa2 sa1 (x :: f3 a3 a2 a1) ->
       case dict3 sa3 sa2 sa1 :: Dict (Ae.ToJSON (f3 a3 a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa3, fromSing sa2, fromSing sa1), x)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> *)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.ToJSON (DemoteRep k4)
    , Ae.ToJSON (DemoteRep k3)
    , Ae.ToJSON (DemoteRep k2)
    , Ae.ToJSON (DemoteRep k1)
    , Dict4 Ae.ToJSON f4
    ) => Ae.ToJSON (Some4 f4)
  where
    {-# INLINABLE toJSON #-}
    toJSON = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f4 a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Ae.ToJSON (f4 a4 a3 a2 a1)) of
          Dict -> Ae.toJSON ((fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1), x)

--------------------------------------------------------------------------------

instance forall (f1 :: k1 -> *)
  . ( SingKind k1
    , Ae.FromJSON (DemoteRep k1)
    , Dict1 Ae.FromJSON f1
    ) => Ae.FromJSON (Some1 f1)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      (rsa1, v') <- Ae.parseJSON v
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
         case dict1 sa1 :: Dict (Ae.FromJSON (f1 a1)) of
            Dict -> do
               x :: f1 a1 <- Ae.parseJSON v'
               return (some1 x)

instance forall (f2 :: k2 -> k1 -> *)
  . ( SingKind k2
    , SingKind k1
    , Ae.FromJSON (DemoteRep k2)
    , Ae.FromJSON (DemoteRep k1)
    , Dict2 Ae.FromJSON f2
    ) => Ae.FromJSON (Some2 f2)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
         withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
            case dict2 sa2 sa1 :: Dict (Ae.FromJSON (f2 a2 a1)) of
               Dict -> do
                  x :: f2 a2 a1 <- Ae.parseJSON v'
                  return (some2 x)

instance forall (f3 :: k3 -> k2 -> k1 -> *)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.FromJSON (DemoteRep k3)
    , Ae.FromJSON (DemoteRep k2)
    , Ae.FromJSON (DemoteRep k1)
    , Dict3 Ae.FromJSON f3
    ) => Ae.FromJSON (Some3 f3)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa3, rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) -> withSingI sa3 $
         withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
            withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
               case dict3 sa3 sa2 sa1 :: Dict (Ae.FromJSON (f3 a3 a2 a1)) of
                  Dict -> do
                     x :: f3 a3 a2 a1 <- Ae.parseJSON v'
                     return (some3 x)

instance forall (f4 :: k4 -> k3 -> k2 -> k1 -> *)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , Ae.FromJSON (DemoteRep k4)
    , Ae.FromJSON (DemoteRep k3)
    , Ae.FromJSON (DemoteRep k2)
    , Ae.FromJSON (DemoteRep k1)
    , Dict4 Ae.FromJSON f4
    ) => Ae.FromJSON (Some4 f4)
  where
    {-# INLINABLE parseJSON #-}
    parseJSON = \v -> do
      ((rsa4, rsa3, rsa2, rsa1), v') <- Ae.parseJSON v
      withSomeSing rsa4 $ \(sa4 :: Sing (a4 :: k4)) -> withSingI sa4 $
         withSomeSing rsa3 $ \(sa3 :: Sing (a3 :: k3)) -> withSingI sa3 $
            withSomeSing rsa2 $ \(sa2 :: Sing (a2 :: k2)) -> withSingI sa2 $
               withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) -> withSingI sa1 $
                  case dict4 sa4 sa3 sa2 sa1 :: Dict (Ae.FromJSON (f4 a4 a3 a2 a1)) of
                     Dict -> do
                        x :: f4 a4 a3 a2 a1 <- Ae.parseJSON v'
                        return (some4 x)
