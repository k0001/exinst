{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}


module Exinst.Internal
 ( -- * 1 type index
   Some1(Some1)
 , some1
 , fromSome1
 , _Some1
 , withSome1
 , withSome1Sing
 , some1SingRep
 , same1
 , Dict1(dict1)

   -- * 2 type indexes
 , Some2(Some2)
 , some2
 , fromSome2
 , _Some2
 , withSome2
 , withSome2Sing
 , some2SingRep
 , same2
 , Dict2(dict2)

   -- * 3 type indexes
 , Some3(Some3)
 , some3
 , fromSome3
 , _Some3
 , withSome3
 , withSome3Sing
 , some3SingRep
 , same3
 , Dict3(dict3)

   -- * 4 type indexes
 , Some4(Some4)
 , some4
 , fromSome4
 , _Some4
 , withSome4
 , withSome4Sing
 , some4SingRep
 , same4
 , Dict4(dict4)

   -- * Miscellaneous
 , Dict0(dict0)
 ) where

import Data.Constraint
import Data.Kind (Type)
import Data.Profunctor (dimap, Choice(right'))
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
      , SingI a1, SDecide k1 )
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
      , SingI a1, SDecide k1 )
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
      , SingI a1, SDecide k1 )
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

-- A @lens@-compatible 'Prism'' for constructing and deconstructing a 'Some1'.
_Some1
  :: forall (f1 :: k1 -> Type) a1
  .  (SingI a1, SDecide k1)
  => Prism' (Some1 f1) (f1 a1)
_Some1 = prism' some1 fromSome1
{-# INLINE _Some1 #-}

-- A @lens@-compatible 'Prism'' for constructing and deconstructing a 'Some2'.
_Some2
  :: forall (f2 :: k2 -> k1 -> Type) a2 a1
  .  ( SingI a2, SDecide k2
     , SingI a1, SDecide k1 )
  => Prism' (Some2 f2) (f2 a2 a1)
_Some2 = prism' some2 fromSome2
{-# INLINE _Some2 #-}

-- A @lens@-compatible 'Prism'' for constructing and deconstructing a 'Some3'.
_Some3
  :: forall (f3 :: k3 -> k2 -> k1 -> Type) a3 a2 a1
  .  ( SingI a3, SDecide k3
     , SingI a2, SDecide k2
     , SingI a1, SDecide k1 )
  => Prism' (Some3 f3) (f3 a3 a2 a1)
_Some3 = prism' some3 fromSome3
{-# INLINE _Some3 #-}

-- A @lens@-compatible 'Prism'' for constructing and deconstructing a 'Some4'.
_Some4
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> Type) a4 a3 a2 a1
  .  ( SingI a4, SDecide k4
     , SingI a3, SDecide k3
     , SingI a2, SDecide k2
     , SingI a1, SDecide k1 )
  => Prism' (Some4 f4) (f4 a4 a3 a2 a1)
_Some4 = prism' some4 fromSome4
{-# INLINE _Some4 #-}

--------------------------------------------------------------------------------

some1SingRep
  :: SingKind k1
  => Some1 (f1 :: k1 -> Type)
  -> DemoteRep k1 -- ^
some1SingRep = \(Some1 sa1 _) -> fromSing sa1
{-# INLINE some1SingRep #-}

some2SingRep
  :: (SingKind k2, SingKind k1)
  => Some2 (f2 :: k2 -> k1 -> Type)
  -> (DemoteRep k2, DemoteRep k1) -- ^
some2SingRep = \(Some2 sa2 sa1 _) -> (fromSing sa2, fromSing sa1)
{-# INLINE some2SingRep #-}

some3SingRep
  :: (SingKind k3, SingKind k2, SingKind k1)
  => Some3 (f3 :: k3 -> k2 -> k1 -> Type)
  -> (DemoteRep k3, DemoteRep k2, DemoteRep k1) -- ^
some3SingRep = \(Some3 sa3 sa2 sa1 _) ->
  (fromSing sa3, fromSing sa2, fromSing sa1)
{-# INLINE some3SingRep #-}

some4SingRep
  :: (SingKind k4, SingKind k3, SingKind k2, SingKind k1)
  => Some4 (f4 :: k4 -> k3 -> k2 -> k1 -> Type)
  -> (DemoteRep k4, DemoteRep k3, DemoteRep k2, DemoteRep k1) -- ^
some4SingRep = \(Some4 sa4 sa3 sa2 sa1 _) ->
  (fromSing sa4, fromSing sa3, fromSing sa2, fromSing sa1)
{-# INLINE some4SingRep #-}

--------------------------------------------------------------------------------

-- | @'same1' x a b@ applies @x@ to the contents of @a@ and @b@ if their type
-- indexes are equal.
--
-- Hint: @'same1' ('some1' . 'Exinst.P1') :: 'Some1' f -> 'Some1' g -> 'Some1' ('Exinst.P1' f g)@
{-# INLINABLE same1 #-}
same1
  :: SDecide k1
  => (forall a1. SingI a1 => f a1 -> g a1 -> x)
  -> Some1 (f :: k1 -> Type)
  -> Some1 (g :: k1 -> Type)
  -> Maybe x  -- ^
same1 z = \s1f s1g ->
  withSome1Sing s1f $ \sa1 f ->
    withSome1Sing s1g $ \sa1' g -> do
       Refl <- testEquality sa1 sa1'
       pure (z f g)

-- | @'same2' x a b@ applies @x@ to the contents of @a@ and @b@ if their type
-- indexes are equal.
--
-- Hint: @'same2' ('some2' . 'Exinst.P2') :: 'Some2' f -> 'Some2' g -> 'Some2' ('Exinst.P2' f g)@
{-# INLINABLE same2 #-}
same2
  :: (SDecide k2, SDecide k1)
  => (forall a2 a1. SingI a1 => f a2 a1 -> g a2 a1 -> x)
  -> Some2 (f :: k2 -> k1 -> Type)
  -> Some2 (g :: k2 -> k1 -> Type)
  -> Maybe x  -- ^
same2 z = \s2l s2g ->
  withSome2Sing s2l $ \sa2 sa1 f ->
    withSome2Sing s2g $ \sa2' sa1' g -> do
       Refl <- testEquality sa2 sa2'
       Refl <- testEquality sa1 sa1'
       pure (z f g)

-- | @'same3' x a b@ applies @x@ to the contents of @a@ and @b@ if their type
-- indexes are equal.
--
-- Hint: @'same3' ('some3' . 'Exinst.P3') :: 'Some3' f -> 'Some3' g -> 'Some3' ('Exinst.P3' f g)@
{-# INLINABLE same3 #-}
same3
  :: (SDecide k3, SDecide k2, SDecide k1)
  => (forall a3 a2 a1. (SingI a3, SingI a2, SingI a1)
        => f a3 a2 a1 -> g a3 a2 a1 -> x)
  -> Some3 (f :: k3 -> k2 -> k1 -> Type)
  -> Some3 (g :: k3 -> k2 -> k1 -> Type)
  -> Maybe x  -- ^
same3 z = \s3l s3g ->
  withSome3Sing s3l $ \sa3 sa2 sa1 f ->
    withSome3Sing s3g $ \sa3' sa2' sa1' g -> do
       Refl <- testEquality sa3 sa3'
       Refl <- testEquality sa2 sa2'
       Refl <- testEquality sa1 sa1'
       pure (z f g)

-- | @'same4' x a b@ applies @x@ to the contents of @a@ and @b@ if their type
-- indexes are equal.
--
-- Hint: @'same4' ('some4' . 'Exinst.P4') :: 'Some4' f -> 'Some4' g -> 'Some4' ('Exinst.P4' f g)@
{-# INLINABLE same4 #-}
same4
  :: (SDecide k4, SDecide k3, SDecide k2, SDecide k1)
  => (forall a4 a3 a2 a1. (SingI a4, SingI a3, SingI a2, SingI a1)
        => f a4 a3 a2 a1 -> g a4 a3 a2 a1 -> x)
  -> Some4 (f :: k4 -> k3 -> k2 -> k1 -> Type)
  -> Some4 (g :: k4 -> k3 -> k2 -> k1 -> Type)
  -> Maybe x  -- ^
same4 z = \s4l s4g ->
  withSome4Sing s4l $ \sa4 sa3 sa2 sa1 f ->
    withSome4Sing s4g $ \sa4' sa3' sa2' sa1' g -> do
       Refl <- testEquality sa4 sa4'
       Refl <- testEquality sa3 sa3'
       Refl <- testEquality sa2 sa2'
       Refl <- testEquality sa1 sa1'
       pure (z f g)

--------------------------------------------------------------------------------

-- | 'Dict0' is a bit different from 'Dict1', 'Dict2', etc. in that it looks up
-- an instance for the singleton type itself, and not for some other type
-- indexed by said singleton type.
class Dict0 (c :: k0 -> Constraint) where
  -- | Runtime lookup of the @c a0@ instance.
  dict0 :: Sing a0 -> Dict (c a0)

class Dict1 (c :: k0 -> Constraint) (f1 :: k1 -> k0) where
  -- | Runtime lookup of the @c (f1 a1)@ instance.
  dict1 :: Sing a1 -> Dict (c (f1 a1))

class Dict2 (c :: k0 -> Constraint) (f2 :: k2 -> k1 -> k0) where
  -- Runtime lookup of the @c (f2 a2 a1)@ instance.
  dict2 :: Sing a2 -> Sing a1 -> Dict (c (f2 a2 a1))

class Dict3 (c :: k0 -> Constraint) (f3 :: k3 -> k2 -> k1 -> k0) where
  -- Runtime lookup of the @c (f3 a3 a2 a1)@ instance.
  dict3 :: Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f3 a3 a2 a1))

class Dict4 (c :: k0 -> Constraint) (f4 :: k4 -> k3 -> k2 -> k1 -> k0) where
  -- Runtime lookup of the @c (f4 a4 a3 a2 a1)@ instance.
  dict4 :: Sing a4 -> Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f4 a4 a3 a2 a1))

--------------------------------------------------------------------------------
-- Miscelaneous @lens@-compatible stuff.

type Prism s t a b
  = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

