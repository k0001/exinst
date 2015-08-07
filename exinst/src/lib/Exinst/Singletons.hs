{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Exinst.Singletons
 ( -- * 1 type index
   Some1
 , mkSome1
 , withSome1
 , withSome1I
 , fromSome1
 , Dict1(dict1)

   -- * 2 type indexes
 , Some2
 , mkSome2
 , withSome2
 , withSome2I
 , fromSome2
 , Dict2(dict2)

   -- * 3 type indexes
 , Some3
 , mkSome3
 , withSome3
 , withSome3I
 , fromSome3
 , Dict3(dict3)

   -- * 4 type indexes
 , Some4
 , mkSome4
 , withSome4
 , withSome4I
 , fromSome4
 , Dict4(dict4)
 ) where

import Data.Constraint
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Types
import Prelude

import Exinst.Singletons.Internal

--------------------------------------------------------------------------------

mkSome1
  :: forall (f1 :: k1 -> *) a1
  .  SingI a1
  => f1 a1
  -> Some1 f1 -- ^
mkSome1 = Some1 (sing :: Sing a1)
{-# INLINE mkSome1 #-}

mkSome2
  :: forall (f2 :: k2 -> k1 -> *) a2 a1
  .  (SingI a2, SingI a1)
  => f2 a2 a1
  -> Some2 f2 -- ^
mkSome2 = Some2 (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE mkSome2 #-}

mkSome3
  :: forall (f3 :: k3 -> k2 -> k1 -> *) a3 a2 a1
  .  (SingI a3, SingI a2, SingI a1)
  => f3 a3 a2 a1
  -> Some3 f3 -- ^
mkSome3 = Some3 (sing :: Sing a3) (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE mkSome3 #-}

mkSome4
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> *) a4 a3 a2 a1
  .  (SingI a4, SingI a3, SingI a2, SingI a1)
  => f4 a4 a3 a2 a1
  -> Some4 f4 -- ^
mkSome4 = Some4 (sing :: Sing a4) (sing :: Sing a3)
                (sing :: Sing a2) (sing :: Sing a1)
{-# INLINE mkSome4 #-}

--------------------------------------------------------------------------------

withSome1I
  :: forall (f1 :: k1 -> *) (r :: *)
   . Some1 f1
  -> (forall a1. SingI a1 => f1 a1 -> r)
  -> r -- ^
withSome1I (Some1 sa1 x) g = withSingI sa1 (g x)
{-# INLINABLE withSome1I #-}

withSome2I
  :: forall (f2 :: k2 -> k1 -> *) (r :: *)
  .  Some2 f2
  -> (forall a2 a1. (SingI a2, SingI a1) => f2 a2 a1 -> r)
  -> r -- ^
withSome2I (Some2 sa2 sa1 x) g = withSingI sa2 (withSingI sa1 (g x))
{-# INLINABLE withSome2I #-}

withSome3I
  :: forall (f3 :: k3 -> k2 -> k1 -> *) (r :: *)
  .  Some3 f3
  -> (forall a3 a2 a1. (SingI a3, SingI a2, SingI a1) => f3 a3 a2 a1 -> r)
  -> r -- ^
withSome3I (Some3 sa3 sa2 sa1 x) g =
  withSingI sa3 (withSingI sa2 (withSingI sa1 (g x)))
{-# INLINABLE withSome3I #-}

withSome4I
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> *) (r :: *)
  .  Some4 f4
  -> (forall a4 a3 a2 a1
        .  (SingI a4, SingI a3, SingI a2, SingI a1)
        => f4 a4 a3 a2 a1 -> r)
  -> r -- ^
withSome4I (Some4 sa4 sa3 sa2 sa1 x) g =
  withSingI sa4 (withSingI sa3 (withSingI sa2 (withSingI sa1 (g x))))
{-# INLINABLE withSome4I #-}

--------------------------------------------------------------------------------

-- | Like 'withSome1I', but takes an explicit 'Sing' instead of a 'SingI' instance.
withSome1
  :: forall (f1 :: k1 -> *) (r :: *)
   . Some1 f1
  -> (forall a1. Sing a1 -> f1 a1 -> r)
  -> r -- ^
withSome1 (Some1 sa1 x) g = g sa1 x
{-# INLINABLE withSome1 #-}

-- | Like 'withSome2I', but takes explicit 'Sing's instead of 'SingI' instances.
withSome2
  :: forall (f2 :: k2 -> k1 -> *) (r :: *)
  .  Some2 f2
  -> (forall a2 a1. Sing a2 -> Sing a1 -> f2 a2 a1 -> r)
  -> r -- ^
withSome2 (Some2 sa2 sa1 x) g = g sa2 sa1 x
{-# INLINABLE withSome2 #-}

-- | Like 'withSome3I', but takes explicit 'Sing's instead of 'SingI' instances.
withSome3
  :: forall (f3 :: k3 -> k2 -> k1 -> *) (r :: *)
  .  Some3 f3
  -> (forall a3 a2 a1. Sing a3 -> Sing a2 -> Sing a1 -> f3 a3 a2 a1 -> r)
  -> r -- ^
withSome3 (Some3 sa3 sa2 sa1 x) g = g sa3 sa2 sa1 x
{-# INLINABLE withSome3 #-}

-- | Like 'withSome4I', but takes explicit 'Sing's instead of 'SingI' instances.
withSome4
  :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> *) (r :: *)
  .  Some4 f4
  -> (forall a4 a3 a2 a1
        . Sing a4 -> Sing a3 -> Sing a2 -> Sing a1 -> f4 a4 a3 a2 a1 -> r)
  -> r -- ^
withSome4 (Some4 sa4 sa3 sa2 sa1 x) g = g sa4 sa3 sa2 sa1 x
{-# INLINABLE withSome4 #-}

--------------------------------------------------------------------------------

fromSome1
   :: forall (f1 :: k1 -> *) a1
    . (SingI a1, SDecide ('KProxy :: KProxy k1))
   => Some1 f1
   -> Maybe (f1 a1) -- ^
fromSome1 = \(Some1 sa1' x) -> do
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome1 #-}

fromSome2
   :: forall (f2 :: k2 -> k1 -> *) a2 a1
    . ( SingI a2, SDecide ('KProxy :: KProxy k2)
      , SingI a1, SDecide ('KProxy :: KProxy k1))
   => Some2 f2
   -> Maybe (f2 a2 a1) -- ^
fromSome2 = \(Some2 sa2' sa1' x) -> do
   Refl <- testEquality sa2' (sing :: Sing a2)
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome2 #-}

fromSome3
   :: forall (f3 :: k3 -> k2 -> k1 -> *) a3 a2 a1
    . ( SingI a3, SDecide ('KProxy :: KProxy k3)
      , SingI a2, SDecide ('KProxy :: KProxy k2)
      , SingI a1, SDecide ('KProxy :: KProxy k1))
   => Some3 f3
   -> Maybe (f3 a3 a2 a1) -- ^
fromSome3 = \(Some3 sa3' sa2' sa1' x) -> do
   Refl <- testEquality sa3' (sing :: Sing a3)
   Refl <- testEquality sa2' (sing :: Sing a2)
   Refl <- testEquality sa1' (sing :: Sing a1)
   return x
{-# INLINABLE fromSome3 #-}

fromSome4
   :: forall (f4 :: k4 -> k3 -> k2 -> k1 -> *) a4 a3 a2 a1
    . ( SingI a4, SDecide ('KProxy :: KProxy k4)
      , SingI a3, SDecide ('KProxy :: KProxy k3)
      , SingI a2, SDecide ('KProxy :: KProxy k2)
      , SingI a1, SDecide ('KProxy :: KProxy k1))
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

class Dict1 (c :: * -> Constraint) (f1 :: k1 -> *) where
  dict1 :: Sing a1 -> Dict (c (f1 a1))

class Dict2 (c :: * -> Constraint) (f2 :: k2 -> k1 -> *) where
  dict2 :: Sing a2 -> Sing a1 -> Dict (c (f2 a2 a1))

class Dict3 (c :: * -> Constraint) (f3 :: k3 -> k2 -> k1 -> *) where
  dict3 :: Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f3 a3 a2 a1))

class Dict4 (c :: * -> Constraint) (f4 :: k4 -> k3 -> k2 -> k1 -> *) where
  dict4 :: Sing a4 -> Sing a3 -> Sing a2 -> Sing a1 -> Dict (c (f4 a4 a3 a2 a1))
