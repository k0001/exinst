{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

-- | See the README file for documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst.Singletons
 ( -- * 1 type index
   Some1(Some1)
 , some1
 , withSome1Sing
 , withSome1
 , fromSome1
 , Dict1(dict1)

   -- * 2 type indexes
 , Some2(Some2)
 , some2
 , withSome2Sing
 , withSome2
 , fromSome2
 , Dict2(dict2)

   -- * 3 type indexes
 , Some3(Some3)
 , some3
 , withSome3Sing
 , withSome3
 , fromSome3
 , Dict3(dict3)

   -- * 4 type indexes
 , Some4(Some4)
 , some4
 , withSome4Sing
 , withSome4
 , fromSome4
 , Dict4(dict4)

   -- * Re-exports
 , Dict(Dict)
 ) where

import Data.Constraint
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.Decide
import Data.Type.Equality
import Prelude

--------------------------------------------------------------------------------

data Some1 (f1 :: k1 -> Type) = forall a1.
  Some1 !(Sing a1) !(f1 a1)

data Some2 (f2 :: k2 -> k1 -> Type) = forall a2 a1.
  Some2 !(Sing a2) !(Sing a1) !(f2 a2 a1)

data Some3 (f3 :: k3 -> k2 -> k1 -> Type) = forall a3 a2 a1.
  Some3 !(Sing a3) !(Sing a2) !(Sing a1) !(f3 a3 a2 a1)

data Some4 (f4 :: k4 -> k3 -> k2 -> k1 -> Type) = forall a4 a3 a2 a1.
  Some4 !(Sing a4) !(Sing a3) !(Sing a2) !(Sing a1) !(f4 a4 a3 a2 a1)

--------------------------------------------------------------------------------

some1
  :: forall (f1 :: k1 -> Type) a1
  .  SingI a1
  => f1 a1
  -> Some1 f1 -- ^
some1 = Some1 (sing :: Sing a1)
{-# INLINE some1 #-}

some2
  :: forall (f2 :: k2 -> k1 -> Type) a2 a1
  .  (SingI a2, SingI a1)
  => f2 a2 a1
  -> Some2 f2 -- ^
some2 = Some2 (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE some2 #-}

some3
  :: forall (f3 :: k3 -> k2 -> k1 -> Type) a3 a2 a1
  .  (SingI a3, SingI a2, SingI a1)
  => f3 a3 a2 a1
  -> Some3 f3 -- ^
some3 = Some3 (sing :: Sing a3) (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE some3 #-}

some4
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type) a4 a3 a2 a1
  .  (SingI a4, SingI a3, SingI a2, SingI a1)
  => f4 a4 a3 a2 a1
  -> Some4 f4 -- ^
some4 = Some4 (sing :: Sing a4) (sing :: Sing a3)
              (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE some4 #-}

--------------------------------------------------------------------------------

withSome1
  :: forall (f1 :: k1 -> Type) (r :: Type)
   . Some1 f1
  -> (forall a1. SingI a1 => f1 a1 -> r)
  -> r -- ^
withSome1 s1 g = withSome1Sing s1 (\_ -> g)
{-# INLINABLE withSome1 #-}

withSome2
  :: forall (f2 :: k2 -> k1 -> Type) (r :: Type)
  .  Some2 f2
  -> (forall a2 a1. (SingI a2, SingI a1) => f2 a2 a1 -> r)
  -> r -- ^
withSome2 s2 g = withSome2Sing s2 (\_ _ -> g)
{-# INLINABLE withSome2 #-}

withSome3
  :: forall (f3 :: k3 -> k2 -> k1 -> Type) (r :: Type)
  .  Some3 f3
  -> (forall a3 a2 a1. (SingI a3, SingI a2, SingI a1) => f3 a3 a2 a1 -> r)
  -> r -- ^
withSome3 s3 g = withSome3Sing s3 (\_ _ _ -> g)
{-# INLINABLE withSome3 #-}

withSome4
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type) (r :: Type)
  .  Some4 f4
  -> (forall a4 a3 a2 a1
        .  (SingI a4, SingI a3, SingI a2, SingI a1)
        => f4 a4 a3 a2 a1 -> r)
  -> r -- ^
withSome4 s4 g = withSome4Sing s4 (\_ _ _ _ -> g)
{-# INLINABLE withSome4 #-}

--------------------------------------------------------------------------------

-- | Like 'withSome1', but takes an explicit 'Sing' besides the 'SingI' instance.
withSome1Sing
  :: forall (f1 :: k1 -> Type) (r :: Type)
   . Some1 f1
  -> (forall a1. (SingI a1) => Sing a1 -> f1 a1 -> r)
  -> r -- ^
withSome1Sing (Some1 sa1 x) g = withSingI sa1 (g sa1 x)
{-# INLINABLE withSome1Sing #-}

-- | Like 'withSome2', but takes explicit 'Sing's besides the 'SingI' instances.
withSome2Sing
  :: forall (f2 :: k2 -> k1 -> Type) (r :: Type)
  .  Some2 f2
  -> (forall a2 a1. (SingI a2, SingI a1) => Sing a2 -> Sing a1 -> f2 a2 a1 -> r)
  -> r -- ^
withSome2Sing (Some2 sa2 sa1 x) g = withSingI sa2 (withSingI sa1 (g sa2 sa1 x))
{-# INLINABLE withSome2Sing #-}

-- | Like 'withSome3', but takes explicit 'Sing's besides the 'SingI' instances.
withSome3Sing
  :: forall (f3 :: k3 -> k2 -> k1 -> Type) (r :: Type)
  .  Some3 f3
  -> (forall a3 a2 a1
         .  (SingI a3, SingI a2, SingI a1)
         => Sing a3 -> Sing a2 -> Sing a1 -> f3 a3 a2 a1 -> r)
  -> r -- ^
withSome3Sing (Some3 sa3 sa2 sa1 x) g =
  withSingI sa3 (withSingI sa2 (withSingI sa1 (g sa3 sa2 sa1 x)))
{-# INLINABLE withSome3Sing #-}

-- | Like 'withSome4', but takes explicit 'Sing's besides the 'SingI' instances.
withSome4Sing
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type) (r :: Type)
  .  Some4 f4
  -> (forall a4 a3 a2 a1
        .  (SingI a4, SingI a3, SingI a2, SingI a1)
        => Sing a4 -> Sing a3 -> Sing a2 -> Sing a1 -> f4 a4 a3 a2 a1 -> r)
  -> r -- ^
withSome4Sing (Some4 sa4 sa3 sa2 sa1 x) g =
  withSingI sa4 (withSingI sa3 (withSingI sa2 (withSingI sa1
     (g sa4 sa3 sa2 sa1 x))))
{-# INLINABLE withSome4Sing #-}

--------------------------------------------------------------------------------

fromSome1
   :: forall (f1 :: k1 -> Type) a1
    . (SingI a1, SDecide k1)
   => Some1 f1
   -> Maybe (f1 a1) -- ^
fromSome1 = \(Some1 sa1' x) -> do
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome1 #-}

fromSome2
   :: forall (f2 :: k2 -> k1 -> Type) a2 a1
    . ( SingI a2, SDecide k2
      , SingI a1, SDecide k1)
   => Some2 f2
   -> Maybe (f2 a2 a1) -- ^
fromSome2 = \(Some2 sa2' sa1' x) -> do
   Refl <- testEquality sa2' (sing :: Sing a2)
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome2 #-}

fromSome3
   :: forall (f3 :: k3 -> k2 -> k1 -> Type) a3 a2 a1
    . ( SingI a3, SDecide k3
      , SingI a2, SDecide k2
      , SingI a1, SDecide k1)
   => Some3 f3
   -> Maybe (f3 a3 a2 a1) -- ^
fromSome3 = \(Some3 sa3' sa2' sa1' x) -> do
   Refl <- testEquality sa3' (sing :: Sing a3)
   Refl <- testEquality sa2' (sing :: Sing a2)
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome3 #-}

fromSome4
   :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type) a4 a3 a2 a1
    . ( SingI a4, SDecide k4
      , SingI a3, SDecide k3
      , SingI a2, SDecide k2
      , SingI a1, SDecide k1)
   => Some4 f4
   -> Maybe (f4 a4 a3 a2 a1) -- ^
fromSome4 = \(Some4 sa4' sa3' sa2' sa1' x) -> do
   Refl <- testEquality sa4' (sing :: Sing a4)
   Refl <- testEquality sa3' (sing :: Sing a3)
   Refl <- testEquality sa2' (sing :: Sing a2)
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome4 #-}

--------------------------------------------------------------------------------

class Dict1 (c :: k0 -> Constraint) (f1 :: k1 -> k0) where
  dict1 :: Sing a1 -> Dict (c (f1 a1))

class Dict2 (c :: k0 -> Constraint) (f2 :: k2 -> k1 -> k0) where
  dict2 :: Sing a2 -> Sing a1 -> Dict (c (f2 a2 a1))

class Dict3 (c :: k0 -> Constraint) (f3 :: k3 -> k2 -> k1 -> k0) where
  dict3 :: Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f3 a3 a2 a1))

class Dict4 (c :: k0 -> Constraint) (f4 :: k4 -> k3 -> k2 -> k1 -> k0) where
  dict4 :: Sing a4 -> Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f4 a4 a3 a2 a1))
