{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}

-- | This is an internal module, do not import it directly. Import
-- "Exinst.Singletons" instead.
module Exinst.Singletons.Internal
 ( Some1(..)
 , Some2(..)
 , Some3(..)
 , Some4(..)
 ) where

import Data.Singletons (Sing)

--------------------------------------------------------------------------------

data Some1 (f1 :: k1 -> *) = forall a1.
  Some1 !(Sing a1) (f1 a1)

data Some2 (f2 :: k2 -> k1 -> *) = forall a2 a1.
  Some2 !(Sing a2) !(Sing a1) (f2 a2 a1)

data Some3 (f3 :: k3 -> k2 -> k1 -> *) = forall a3 a2 a1.
  Some3 !(Sing a3) !(Sing a2) !(Sing a1) (f3 a3 a2 a1)

data Some4 (f4 :: k4 -> k3 -> k2 -> k1 -> *) = forall a4 a3 a2 a1.
  Some4 !(Sing a4) !(Sing a3) !(Sing a2) !(Sing a1) (f4 a4 a3 a2 a1)

--------------------------------------------------------------------------------
