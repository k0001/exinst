{-# LANGUAGE CPP #-}

-- | See the README file for documentation: https://hackage.haskell.org/package/exinst#readme
module Exinst
 ( -- * 1 type index
   Some1(Some1)
 , some1
 , fromSome1
 , _Some1
 , withSome1
 , withSome1Sing
 , some1SingRep
 , Dict1(dict1)

   -- * 2 type indexes
 , Some2(Some2)
 , some2
 , fromSome2
 , _Some2
 , withSome2
 , withSome2Sing
 , some2SingRep
 , Dict2(dict2)

   -- * 3 type indexes
 , Some3(Some3)
 , some3
 , fromSome3
 , _Some3
 , withSome3
 , withSome3Sing
 , some3SingRep
 , Dict3(dict3)

   -- * 4 type indexes
 , Some4(Some4)
 , some4
 , fromSome4
 , _Some4
 , withSome4
 , withSome4Sing
 , some4SingRep
 , Dict4(dict4)

   -- * Miscellaneous
 , Dict0(dict0)

   -- * Products
 , P1(P1)
 , P2(P2)
 , P3(P3)
 , P4(P4)

   -- * Sums
 , S1(S1L,S1R)
 , S2(S2L,S2R)
 , S3(S3L,S3R)
 , S4(S4L,S4R)

   -- * Re-exports
 , Constraint
 , Dict(Dict)
 ) where

import Data.Constraint (Constraint, Dict(Dict))

import Exinst.Internal
import Exinst.Internal.Product
import Exinst.Internal.Sum

import Exinst.Instances.Base ()

#ifdef VERSION_aeson
import Exinst.Instances.Aeson ()
#endif

#ifdef VERSION_binary
import Exinst.Instances.Binary ()
#endif

#ifdef VERSION_bytes
import Exinst.Instances.Bytes ()
#endif

#ifdef VERSION_cereal
import Exinst.Instances.Cereal ()
#endif

#ifdef VERSION_deepseq
import Exinst.Instances.DeepSeq ()
#endif

#ifdef VERSION_hashable
import Exinst.Instances.Hashable ()
#endif

#ifdef VERSION_QuickCheck
import Exinst.Instances.QuickCheck ()
#endif
