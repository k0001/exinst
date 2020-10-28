{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports 'Show', 'Eq' and 'Ord' instances for 'Exinst.Some1',
-- 'Exinst.Some2', 'Exinst.Some3' and 'Exinst.Some4' from "Exinst", provided situable
-- 'Dict1', 'Dict2', 'Dict3' and 'Dict4' instances are available.
--
-- See the README file for more general documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst.Base () where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.Prelude.Enum (PEnum(EnumFromTo), PBounded(MinBound, MaxBound))
import Data.Singletons.Prelude.Bool (SBool(STrue, SFalse))
import qualified Data.Singletons.Prelude.List as List
import Data.Singletons.Prelude.Tuple (Tuple2Sym1)
import Data.Singletons.Decide
import qualified GHC.Generics as G
import Prelude
import qualified Text.Read as Read

import Exinst
  hiding (Some1(..), Some2(..), Some3(..), Some4(..))
import qualified Exinst

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
    ) => Show (Exinst.Some1 f)
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
    ) => Show (Exinst.Some2 f)
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
    ) => Show (Exinst.Some3 f)
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
    ) => Show (Exinst.Some4 f)
  where
    {-# INLINABLE showsPrec #-}
    showsPrec n = \some4x -> withSome4Sing some4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
       case dict4 sa4 sa3 sa2 sa1 :: Dict (Show (f a4 a3 a2 a1)) of
          Dict -> showsPrec n (Some4 (fromSing sa4) (fromSing sa3)
                                     (fromSing sa2) (fromSing sa1) x)

--------------------------------------------------------------------------------
-- Read

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , Read (Demote k1)
    , Dict1 Read f
    ) => Read (Exinst.Some1 f)
  where
    {-# INLINABLE readPrec #-}
    readPrec = do
      Read.Ident "Some1" <- Read.lexP
      rsa1 <- Read.readPrec
      withSomeSing rsa1 $ \(sa1 :: Sing (a1 :: k1)) ->
         case dict1 sa1 :: Dict (Read (f a1)) of
            Dict -> do
               x :: f a1 <- Read.readPrec
               pure (Exinst.Some1 sa1 x)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , Read (Demote k2)
    , Read (Demote k1)
    , Dict2 Read f
    ) => Read (Exinst.Some2 f)
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
                  pure (Exinst.Some2 sa2 sa1 x)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , Read (Demote k3)
    , Read (Demote k2)
    , Read (Demote k1)
    , Dict3 Read f
    ) => Read (Exinst.Some3 f)
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
                     pure (Exinst.Some3 sa3 sa2 sa1 x)

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
    ) => Read (Exinst.Some4 f)
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
                        pure (Exinst.Some4 sa4 sa3 sa2 sa1 x)

--------------------------------------------------------------------------------
-- Eq

instance forall k1 (f :: k1 -> Type).
  ( SDecide k1
  , Dict1 Eq f
  ) => Eq (Exinst.Some1 f)
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
    ) => Eq (Exinst.Some2 f)
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
    ) => Eq (Exinst.Some3 f)
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
    ) => Eq (Exinst.Some4 f)
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

--------------------------------------------------------------------------------
-- Ord

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , SDecide k1
    , Ord (Demote k1)
    , Dict1 Ord f
    , Eq (Exinst.Some1 f)
    ) => Ord (Exinst.Some1 f)
  where
    {-# INLINABLE compare #-}
    compare = \som1x som1y ->
       withSome1Sing som1x $ \sa1x (x :: f a1x) ->
          withSome1Sing som1y $ \sa1y (y :: f a1y) ->
             let termCompare = compare (fromSing sa1x) (fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- decideEquality sa1x sa1y
                  case dict1 sa1x :: Dict (Ord (f a1x)) of
                     Dict -> Just (compare x y)

instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , SDecide k2
    , SDecide k1
    , Ord (Demote k2)
    , Ord (Demote k1)
    , Dict2 Ord f
    , Eq (Exinst.Some2 f)
    ) => Ord (Exinst.Some2 f)
  where
    {-# INLINABLE compare #-}
    compare = \som2x som2y ->
       withSome2Sing som2x $ \sa2x sa1x (x :: f a2x a1x) ->
          withSome2Sing som2y $ \sa2y sa1y (y :: f a2y a1y) ->
             let termCompare = compare (fromSing sa2x, fromSing sa1x)
                                       (fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                   Refl <- decideEquality sa2x sa2y
                   Refl <- decideEquality sa1x sa1y
                   case dict2 sa2x sa1x :: Dict (Ord (f a2x a1x)) of
                      Dict -> Just (compare x y)

instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Ord (Demote k3)
    , Ord (Demote k2)
    , Ord (Demote k1)
    , Dict3 Ord f
    , Eq (Exinst.Some3 f)
    ) => Ord (Exinst.Some3 f)
  where
    {-# INLINABLE compare #-}
    compare = \som3x som3y ->
       withSome3Sing som3x $ \sa3x sa2x sa1x (x :: f a3x a2x a1x) ->
          withSome3Sing som3y $ \sa3y sa2y sa1y (y :: f a3y a2y a1y) ->
             let termCompare = compare
                   (fromSing sa3x, fromSing sa2x, fromSing sa1x)
                   (fromSing sa3y, fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- decideEquality sa3x sa3y
                  Refl <- decideEquality sa2x sa2y
                  Refl <- decideEquality sa1x sa1y
                  case dict3 sa3x sa2x sa1x :: Dict (Ord (f a3x a2x a1x)) of
                     Dict -> Just (compare x y)

instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , SDecide k4
    , SDecide k3
    , SDecide k2
    , SDecide k1
    , Ord (Demote k4)
    , Ord (Demote k3)
    , Ord (Demote k2)
    , Ord (Demote k1)
    , Dict4 Ord f
    , Eq (Exinst.Some4 f)
    ) => Ord (Exinst.Some4 f)
  where
    {-# INLINABLE compare #-}
    compare = \som4x som4y ->
       withSome4Sing som4x $ \sa4x sa3x sa2x sa1x (x :: f a4x a3x a2x a1x) ->
          withSome4Sing som4y $ \sa4y sa3y sa2y sa1y (y :: f a4y a3y a2y a1y) ->
             let termCompare = compare
                   (fromSing sa4x, fromSing sa3x, fromSing sa2x, fromSing sa1x)
                   (fromSing sa4y, fromSing sa3y, fromSing sa2y, fromSing sa1y)
             in maybe termCompare id $ do
                  Refl <- decideEquality sa4x sa4y
                  Refl <- decideEquality sa3x sa3y
                  Refl <- decideEquality sa2x sa2y
                  Refl <- decideEquality sa1x sa1y
                  case dict4 sa4x sa3x sa2x sa1x :: Dict (Ord (f a4x a3x a2x a1x)) of
                     Dict -> Just (compare x y)

--------------------------------------------------------------------------------
-- Generic

type Eithers1 (f :: k1 -> Type) =
  Eithers1' (EnumFromTo (MinBound :: k1) (MaxBound :: k1)) f

-- | TODO: Mak1e this logarithmic.
type family Eithers1' (xs :: [k1]) (f :: k1 -> Type) :: Type where
  Eithers1' (x ': '[]) f = f x
  Eithers1' (x ': xs)  f = Either (f x) (Eithers1' xs f)

instance forall k1 (f :: k1 -> Type)
  . ( SingKind k1
    , PEnum (Demote k1)
    , PBounded (Demote k1)
    , G.Generic (Demote k1)
    , Dict1 G.Generic f
    , Dict1 (Inj (Eithers1 f)) f
    ) => G.Generic (Exinst.Some1 f)
  where
    type Rep (Exinst.Some1 (f :: k1 -> Type)) =
      G.Rep (Demote k1, Eithers1 f)
    {-# INLINABLE from #-}
    from = \s1x -> withSome1Sing s1x $ \sa1 (x :: f a1) ->
      case dict1 sa1 :: Dict (G.Generic (f a1)) of
        Dict -> case dict1 sa1 :: Dict (Inj (Eithers1 f) (f a1)) of
          Dict -> G.from (fromSing sa1, inj x)
    {-# INLINABLE to #-}
    to = \(G.M1 (G.M1 (G.M1 (G.K1 da1) G.:*: G.M1 (G.K1 ex)))) ->
      withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
        case dict1 sa1 :: Dict (Inj (Eithers1 f) (f a1)) of
          Dict -> case prj ex of
            Just x -> Exinst.Some1 sa1 (x :: f a1)
            Nothing -> error "Generic Some1: Malformed Rep"

---
type Eithers2 (f :: k2 -> k1 -> Type) =
  Eithers2' (Cartesian2 (EnumFromTo (MinBound :: k2) (MaxBound :: k2))
                        (EnumFromTo (MinBound :: k1) (MaxBound :: k1))) f

-- | TODO: Mak1e this logarithmic.
type family Eithers2' (xs :: [(k2, k1)]) (f :: k2 -> k1 -> Type) :: Type where
  Eithers2' ( '(x2, x1) ': '[]) f = f x2 x1
  Eithers2' ( '(x2, x1) ': xs)  f = Either (f x2 x1) (Eithers2' xs f)

type family Cartesian2 (xs2 :: [k2]) (xs1 :: [k1]) :: [(k2,k1)] where
  Cartesian2 '[] xs1 = '[]
  Cartesian2 (x2 ': xs2) xs1 =
    List.Concat [List.Map (Tuple2Sym1 x2) xs1, Cartesian2 xs2 xs1]


instance forall k2 k1 (f :: k2 -> k1 -> Type)
  . ( SingKind k2
    , SingKind k1
    , PEnum (Demote k2)
    , PEnum (Demote k1)
    , PBounded (Demote k2)
    , PBounded (Demote k1)
    , G.Generic (Demote k2)
    , G.Generic (Demote k1)
    , Dict2 G.Generic f
    , Dict2 (Inj (Eithers2 f)) f
    ) => G.Generic (Exinst.Some2 f)
  where
    type Rep (Exinst.Some2 (f :: k2 -> k1 -> Type)) =
      G.Rep ((Demote k2, Demote k1), Eithers2 f)
    {-# INLINABLE from #-}
    from = \s2x -> withSome2Sing s2x $ \sa2 sa1 (x :: f a2 a1) ->
      case dict2 sa2 sa1 :: Dict (G.Generic (f a2 a1)) of
        Dict -> case dict2 sa2 sa1 :: Dict (Inj (Eithers2 f) (f a2 a1)) of
          Dict -> G.from ((fromSing sa2, fromSing sa1), inj x)
    {-# INLINABLE to #-}
    to = \(G.M1 (G.M1 (G.M1 (G.K1 (da2, da1)) G.:*: G.M1 (G.K1 ex)))) ->
      withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
        withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
          case dict2 sa2 sa1 :: Dict (Inj (Eithers2 f) (f a2 a1)) of
            Dict -> case prj ex of
              Just x -> Exinst.Some2 sa2 sa1 (x :: f a2 a1)
              Nothing -> error "Generic Some2: Malformed Rep"


---
type Eithers3 (f :: k3 -> k2 -> k1 -> Type) =
  Eithers3' (Cartesian3 (EnumFromTo (MinBound :: k3) (MaxBound :: k3))
                        (EnumFromTo (MinBound :: k2) (MaxBound :: k2))
                        (EnumFromTo (MinBound :: k1) (MaxBound :: k1))) f

-- | TODO: Mak1e this logarithmic.
type family Eithers3' (xs :: [(k3, (k2, k1))]) (f :: k3 -> k2 -> k1 -> Type) :: Type where
  Eithers3' ( '(x3, '(x2, x1)) ': '[]) f = f x3 x2 x1
  Eithers3' ( '(x3, '(x2, x1)) ': xs)  f = Either (f x3 x2 x1) (Eithers3' xs f)

-- | We use nested 2-tuples instead of 3-tuples because it's easier to implement.
type family Cartesian3 (xs3 :: [k3]) (xs2 :: [k2]) (xs1 :: [k1]) :: [(k3,(k2,k1))] where
  Cartesian3 '[] xs2 xs1 = '[]
  Cartesian3 (x3 ': xs3) xs2 xs1 =
    List.Concat [ List.Map (Tuple2Sym1 x3) (Cartesian2 xs2 xs1)
                , Cartesian3 xs3 xs2 xs1 ]


instance forall k3 k2 k1 (f :: k3 -> k2 -> k1 -> Type)
  . ( SingKind k3
    , SingKind k2
    , SingKind k1
    , PEnum (Demote k3)
    , PEnum (Demote k2)
    , PEnum (Demote k1)
    , PBounded (Demote k3)
    , PBounded (Demote k2)
    , PBounded (Demote k1)
    , G.Generic (Demote k3)
    , G.Generic (Demote k2)
    , G.Generic (Demote k1)
    , Dict3 G.Generic f
    , Dict3 (Inj (Eithers3 f)) f
    ) => G.Generic (Exinst.Some3 f)
  where
    type Rep (Exinst.Some3 (f :: k3 -> k2 -> k1 -> Type)) =
      G.Rep ((Demote k3, Demote k2, Demote k1), Eithers3 f)
    {-# INLINABLE from #-}
    from = \s3x -> withSome3Sing s3x $ \sa3 sa2 sa1 (x :: f a3 a2 a1) ->
      case dict3 sa3 sa2 sa1 :: Dict (G.Generic (f a3 a2 a1)) of
        Dict -> case dict3 sa3 sa2 sa1 :: Dict (Inj (Eithers3 f) (f a3 a2 a1)) of
          Dict -> G.from ((fromSing sa3, fromSing sa2, fromSing sa1), inj x)
    {-# INLINABLE to #-}
    to = \(G.M1 (G.M1 (G.M1 (G.K1 (da3, da2, da1)) G.:*: G.M1 (G.K1 ex)))) ->
      withSomeSing da3 $ \(sa3 :: Sing (a3 :: k3)) ->
        withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
          withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
            case dict3 sa3 sa2 sa1 :: Dict (Inj (Eithers3 f) (f a3 a2 a1)) of
              Dict -> case prj ex of
                Just x -> Exinst.Some3 sa3 sa2 sa1 (x :: f a3 a2 a1)
                Nothing -> error "Generic Some3: Malformed Rep"


---
type Eithers4 (f :: k4 -> k3 -> k2 -> k1 -> Type) =
  Eithers4' (Cartesian4 (EnumFromTo (MinBound :: k4) (MaxBound :: k4))
                        (EnumFromTo (MinBound :: k3) (MaxBound :: k3))
                        (EnumFromTo (MinBound :: k2) (MaxBound :: k2))
                        (EnumFromTo (MinBound :: k1) (MaxBound :: k1))) f

-- | TODO: Mak1e this logarithmic.
type family Eithers4' (xs :: [(k4, (k3, (k2, k1)))]) (f :: k4 -> k3 -> k2 -> k1 -> Type) :: Type where
  Eithers4' ( '( x4, '(x3, '(x2, x1))) ': '[]) f = f x4 x3 x2 x1
  Eithers4' ( '( x4, '(x3, '(x2, x1))) ': xs)  f = Either (f x4 x3 x2 x1) (Eithers4' xs f)

-- | We use nested 2-tuples instead of 4-tuples because it's easier to implement.
type family Cartesian4 (xs4 :: [k4]) (xs3 :: [k3]) (xs2 :: [k2]) (xs1 :: [k1]) :: [(k4,(k3,(k2,k1)))] where
  Cartesian4 '[] xs3 xs2 xs1 = '[]
  Cartesian4 (x4 ': xs4) xs3 xs2 xs1 =
    List.Concat [ List.Map (Tuple2Sym1 x4) (Cartesian3 xs3 xs2 xs1)
                , Cartesian4 xs4 xs3 xs2 xs1 ]


instance forall k4 k3 k2 k1 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  . ( SingKind k4
    , SingKind k3
    , SingKind k2
    , SingKind k1
    , PEnum (Demote k4)
    , PEnum (Demote k3)
    , PEnum (Demote k2)
    , PEnum (Demote k1)
    , PBounded (Demote k4)
    , PBounded (Demote k3)
    , PBounded (Demote k2)
    , PBounded (Demote k1)
    , G.Generic (Demote k4)
    , G.Generic (Demote k3)
    , G.Generic (Demote k2)
    , G.Generic (Demote k1)
    , Dict4 G.Generic f
    , Dict4 (Inj (Eithers4 f)) f
    ) => G.Generic (Exinst.Some4 f)
  where
    type Rep (Exinst.Some4 (f :: k4 -> k3 -> k2 -> k1 -> Type)) =
      G.Rep ((Demote k4, Demote k3, Demote k2, Demote k1), Eithers4 f)
    {-# INLINABLE from #-}
    from = \s4x -> withSome4Sing s4x $ \sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1) ->
      case dict4 sa4 sa3 sa2 sa1 :: Dict (G.Generic (f a4 a3 a2 a1)) of
        Dict -> case dict4 sa4 sa3 sa2 sa1 :: Dict (Inj (Eithers4 f) (f a4 a3 a2 a1)) of
          Dict -> G.from ((fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1), inj x)
    {-# INLINABLE to #-}
    to = \(G.M1 (G.M1 (G.M1 (G.K1 (da4, da3, da2, da1)) G.:*: G.M1 (G.K1 ex)))) ->
      withSomeSing da4 $ \(sa4 :: Sing (a4 :: k4)) ->
        withSomeSing da3 $ \(sa3 :: Sing (a3 :: k3)) ->
          withSomeSing da2 $ \(sa2 :: Sing (a2 :: k2)) ->
            withSomeSing da1 $ \(sa1 :: Sing (a1 :: k1)) ->
              case dict4 sa4 sa3 sa2 sa1 :: Dict (Inj (Eithers4 f) (f a4 a3 a2 a1)) of
                Dict -> case prj ex of
                  Just x -> Exinst.Some4 sa4 sa3 sa2 sa1 (x :: f a4 a3 a2 a1)
                  Nothing -> error "Generic Some4: Malformed Rep"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Out of the box 'DictX' instances for some @base@ types

instance forall c.
  (c 'False, c 'True
  ) => Dict0 (c :: Bool -> Constraint) where
  {-# INLINABLE dict0 #-}
  dict0 = \case { SFalse -> Dict; STrue -> Dict }

instance forall k0 c f.
  ( c (f 'False), c (f 'True)
  ) => Dict1 c (f :: Bool -> k0) where
  {-# INLINABLE dict1 #-}
  dict1 = \case { SFalse -> Dict; STrue -> Dict }

instance forall k1 k0 c f.
  ( Dict1 c (f 'False), Dict1 c (f 'True)
  ) => Dict2 c (f :: Bool -> k1 -> k0) where
  {-# INLINABLE dict2 #-}
  dict2 = \x -> case x of { SFalse -> dict1; STrue -> dict1 }

instance forall k2 k1 k0 c f.
  ( Dict2 c (f 'False), Dict2 c (f 'True)
  ) => Dict3 c (f :: Bool -> k2 -> k1 -> k0) where
  {-# INLINABLE dict3 #-}
  dict3 = \x -> case x of { SFalse -> dict2; STrue -> dict2 }

instance forall k3 k2 k1 k0 c f.
  ( Dict3 c (f 'False), Dict3 c (f 'True)
  ) => Dict4 c (f :: Bool -> k3 -> k2 -> k1 -> k0) where
  {-# INLINABLE dict4 #-}
  dict4 = \x -> case x of { SFalse -> dict3; STrue -> dict3 }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Misc

class Inj b a where
  inj :: a -> b
  prj :: b -> Maybe a
instance Inj a a where
  {-# INLINE inj #-}
  inj = id
  {-# INLINE prj #-}
  prj = Just
instance Inj (Either a b) a where
  {-# INLINE inj #-}
  inj = Left
  {-# INLINE prj #-}
  prj = either Just (const Nothing)
-- | TODO: Make this logarithmic.
instance {-# OVERLAPPABLE #-} Inj x a => Inj (Either b x) a where
  {-# INLINE inj #-}
  inj = Right . inj
  {-# INLINE prj #-}
  prj = either (const Nothing) prj

