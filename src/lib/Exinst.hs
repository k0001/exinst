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

   -- * Re-exports
 , Constraint
 , Dict(Dict)
 ) where

import Data.Constraint (Constraint, Dict(Dict))

import Exinst.Internal

#ifdef VERSION_aeson
import Exinst.Instances.Aeson ()
#endif

#ifdef VERSION_bytes
import Exinst.Instances.Bytes ()
#endif

#ifdef VERSION_bytes
import Exinst.Instances.Base ()
#endif

#ifdef VERSION_bytes
import Exinst.Instances.DeepSeq ()
#endif

#ifdef VERSION_bytes
import Exinst.Instances.Hashable ()
#endif

#ifdef VERSION_QuickCheck
import Exinst.Instances.QuickCheck ()
#endif
