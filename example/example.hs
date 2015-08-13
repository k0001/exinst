{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds#-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.DeepSeq
import qualified Data.Aeson as Ae
import qualified Data.Bytes.Get as By
import qualified Data.Bytes.Put as By
import qualified Data.Bytes.Serial as By
import qualified Data.Bytes.Get as By
import qualified Data.Bytes.Put as By
import           Data.Constraint
import           Data.Hashable
import           Data.Singletons.TH
import           GHC.Generics (Generic)
import           Prelude

import           Exinst.Singletons
import           Exinst.Instances.Aeson ()
import           Exinst.Instances.Base ()
import           Exinst.Instances.Bytes ()
import           Exinst.Instances.Hashable ()
import           Exinst.Instances.DeepSeq ()

-- In this example I use GI to derive generic instances, but you don't need to.
import qualified Generics.Instant as GI
import qualified Generics.Instant.TH as GI
import qualified Generics.Instant.Functions.Aeson as GI
import qualified Generics.Instant.Functions.Bytes as GI
import qualified Generics.Instant.Functions.DeepSeq as GI
import qualified Generics.Instant.Functions.Eq as GI
import qualified Generics.Instant.Functions.Hashable as GI
import qualified Generics.Instant.Functions.Show as GI

--------------------------------------------------------------------------------
-- The singleton types that will be used as indexes. The derived instances are
-- requiered by instances for the 'Some1', 'Some2', 'Some3', and 'Some4'
-- existential wrappers.

data T4 = T4a | T4b deriving (Eq, Ord, Show, Generic, By.Serial, Ae.FromJSON, Ae.ToJSON, Hashable)
data T3 = T3a | T3b deriving (Eq, Ord, Show, Generic, By.Serial, Ae.FromJSON, Ae.ToJSON, Hashable)
data T2 = T2a | T2b deriving (Eq, Ord, Show, Generic, By.Serial, Ae.FromJSON, Ae.ToJSON, Hashable)
data T1 = T1a | T1b deriving (Eq, Ord, Show, Generic, By.Serial, Ae.FromJSON, Ae.ToJSON, Hashable)

genSingletons [''T4, ''T3, ''T2, ''T1]
singDecideInstances [''T4, ''T3, ''T2, ''T1]

--------------------------------------------------------------------------------
-- Our example GADT, which we will "existentialize" bit by bit, using the 'Some1',
-- 'Some2', 'Some3' and 'Some4' wrappers.

data X :: T4 -> T3 -> T2 -> T1 -> * where
   Xaaaa :: Int -> X 'T4a 'T3a 'T2a 'T1a
   Xaaab :: Int -> X 'T4a 'T3a 'T2a 'T1b
   Xaaba :: Int -> X 'T4a 'T3a 'T2b 'T1a
   Xaabb :: Int -> X 'T4a 'T3a 'T2b 'T1b
   Xabaa :: Int -> X 'T4a 'T3b 'T2a 'T1a
   Xabab :: Int -> X 'T4a 'T3b 'T2a 'T1b
   Xabba :: Int -> X 'T4a 'T3b 'T2b 'T1a
   Xabbb :: Int -> X 'T4a 'T3b 'T2b 'T1b
   Xbaaa :: Int -> X 'T4b 'T3a 'T2a 'T1a
   Xbaab :: Int -> X 'T4b 'T3a 'T2a 'T1b
   Xbaba :: Int -> X 'T4b 'T3a 'T2b 'T1a
   Xbabb :: Int -> X 'T4b 'T3a 'T2b 'T1b
   Xbbaa :: Int -> X 'T4b 'T3b 'T2a 'T1a
   Xbbab :: Int -> X 'T4b 'T3b 'T2a 'T1b
   Xbbba :: Int -> X 'T4b 'T3b 'T2b 'T1a
   Xbbbb :: Int -> X 'T4b 'T3b 'T2b 'T1b

-- | Hide 1 type index
type Some1X t4 t3 t2 = Some1 (X t4 t3 t2)
-- | Hide 2 type index
type Some2X t4 t3 = Some2 (X t4 t3)
-- | Hide 3 type index
type Some3X t4 = Some3 (X t4)
-- | Hide 4 type index
type Some4X = Some4 X

--------------------------------------------------------------------------------
-- Dictionaries used by instances for 'Some1', 'Some2', 'Some3' and 'Some4'.

instance (c (f1 'T1a), c (f1 'T1b)) => Dict1 c (f1 :: T1 -> *) where
  dict1 = \case { ST1a -> Dict; ST1b -> Dict }
instance (Dict1 c (f2 'T2a), Dict1 c (f2 'T2b)) => Dict2 c (f2 :: T2 -> k1 -> *) where
  dict2 = \case { ST2a -> dict1; ST2b -> dict1 }
instance (Dict2 c (f3 'T3a), Dict2 c (f3 'T3b)) => Dict3 c (f3 :: T3 -> k2 -> k1 -> *) where
  dict3 = \case { ST3a -> dict2; ST3b -> dict2 }
instance (Dict3 c (f4 'T4a), Dict3 c (f4 'T4b)) => Dict4 c (f4 :: T4 -> k3 -> k2 -> k1 -> *) where
  dict4 = \case { ST4a -> dict3; ST4b -> dict3 }

--------------------------------------------------------------------------------

main :: IO ()
main = do
   let x0 = Xabba 42
       x1 = some1 x0
       x2 = some2 x0
       x3 = some3 x0
       x4 = some4 x0
   -- try the Show instances
   print $ x1
   print $ x1
   print $ x2
   print $ x3
   print $ x4
   -- try the Hashable instances
   print $ hash x0
   print $ hash x1
   print $ hash x2
   print $ hash x3
   print $ hash x4
   -- try the NFData instances
   print $!! (Xbaba 321)
   print $!! some1 (Xaaab 634)
   print $!! some2 (Xabbb 111)
   print $!! some3 (Xbbaa 215)
   print $!! some4 (Xbaaa 099)
   -- try the Eq instances
   print $ x0 == x0
   print $ x1 == x1
   print $ x2 == x2
   print $ x3 == x3
   print $ x4 == x4
   -- try the ToJSON instances
   print $ Ae.encode x0
   print $ Ae.encode x1
   print $ Ae.encode x2
   print $ Ae.encode x3
   print $ Ae.encode x4
   -- try the JSON instances (encode, decode, Eq)
   print $ (Ae.decode (Ae.encode x0)) == Just x0
   print $ (Ae.decode (Ae.encode x1)) == Just x1
   print $ (Ae.decode (Ae.encode x2)) == Just x2
   print $ (Ae.decode (Ae.encode x3)) == Just x3
   print $ (Ae.decode (Ae.encode x4)) == Just x4
   -- try the Serial instances (serialize)
   print $ By.runPutL (By.serialize x0)
   print $ By.runPutL (By.serialize x1)
   print $ By.runPutL (By.serialize x2)
   print $ By.runPutL (By.serialize x3)
   print $ By.runPutL (By.serialize x4)
   -- try the Serial instances (serialize, deserialize, Eq)
   print $ (By.runGetL By.deserialize (By.runPutL (By.serialize x0)) == x0)
   print $ (By.runGetL By.deserialize (By.runPutL (By.serialize x1)) == x1)
   print $ (By.runGetL By.deserialize (By.runPutL (By.serialize x2)) == x2)
   print $ (By.runGetL By.deserialize (By.runPutL (By.serialize x3)) == x3)
   print $ (By.runGetL By.deserialize (By.runPutL (By.serialize x4)) == x4)
   -- TODO: try the Ord instances

--------------------------------------------------------------------------------

-- NOTE: Do not pay much attention to the boilerplate below, its just the code
-- giving generic instances to each of the fully applied 'X' type constructor.
-- Everthing here could be automatically derived by TH using GI, but GI doesn't
-- support deriving all of this automatically for GADTs indexed over more than
-- one type. In your use case you might even have defined these instances by hand,
-- so you can mostly ignore this.

data X_Xaaaa_
data X_Xaaab_
data X_Xaaba_
data X_Xaabb_
data X_Xabaa_
data X_Xabab_
data X_Xabba_
data X_Xabbb_
data X_Xbaaa_
data X_Xbaab_
data X_Xbaba_
data X_Xbabb_
data X_Xbbaa_
data X_Xbbab_
data X_Xbbba_
data X_Xbbbb_

instance GI.Constructor X_Xaaaa_ where conName _ = "Xaaaa"
instance GI.Constructor X_Xaaab_ where conName _ = "Xaaab"
instance GI.Constructor X_Xaaba_ where conName _ = "Xaaba"
instance GI.Constructor X_Xaabb_ where conName _ = "Xaabb"
instance GI.Constructor X_Xabaa_ where conName _ = "Xabaa"
instance GI.Constructor X_Xabab_ where conName _ = "Xabab"
instance GI.Constructor X_Xabba_ where conName _ = "Xabba"
instance GI.Constructor X_Xabbb_ where conName _ = "Xabbb"
instance GI.Constructor X_Xbaaa_ where conName _ = "Xbaaa"
instance GI.Constructor X_Xbaab_ where conName _ = "Xbaab"
instance GI.Constructor X_Xbaba_ where conName _ = "Xbaba"
instance GI.Constructor X_Xbabb_ where conName _ = "Xbabb"
instance GI.Constructor X_Xbbaa_ where conName _ = "Xbbaa"
instance GI.Constructor X_Xbbab_ where conName _ = "Xbbab"
instance GI.Constructor X_Xbbba_ where conName _ = "Xbbba"
instance GI.Constructor X_Xbbbb_ where conName _ = "Xbbbb"

type RepX_ (t4 :: T4) (t3 :: T3) (t2 :: T2) (t1 :: T1) =
    ((((GI.CEq X_Xaaaa_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3a, T2a, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xaaab_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3a, T2a, T1b), ()) (GI.Rec Int))) GI.:+:
      ((GI.CEq X_Xaaba_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3a, T2b, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xaabb_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3a, T2b, T1b), ()) (GI.Rec Int)))) GI.:+:
     (((GI.CEq X_Xabaa_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3b, T2a, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xabab_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3b, T2a, T1b), ()) (GI.Rec Int))) GI.:+:
      ((GI.CEq X_Xabba_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3b, T2b, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xabbb_ '( '(t4, t3, t2, t1), ()) '( '(T4a, T3b, T2b, T1b), ()) (GI.Rec Int))))) GI.:+:
    ((((GI.CEq X_Xbaaa_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3a, T2a, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xbaab_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3a, T2a, T1b), ()) (GI.Rec Int))) GI.:+:
      ((GI.CEq X_Xbaba_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3a, T2b, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xbabb_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3a, T2b, T1b), ()) (GI.Rec Int)))) GI.:+:
     (((GI.CEq X_Xbbaa_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3b, T2a, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xbbab_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3b, T2a, T1b), ()) (GI.Rec Int))) GI.:+:
      ((GI.CEq X_Xbbba_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3b, T2b, T1a), ()) (GI.Rec Int)) GI.:+:
       (GI.CEq X_Xbbbb_ '( '(t4, t3, t2, t1), ()) '( '(T4b, T3b, T2b, T1b), ()) (GI.Rec Int)))))

instance GI.Representable (X t4 t3 t2 t1) where
  type Rep (X t4 t3 t2 t1) = RepX_ t4 t3 t2 t1
  from = \case
    Xaaaa a -> GI.L (GI.L (GI.L (GI.L (GI.C (GI.Rec a)))))
    Xaaab a -> GI.L (GI.L (GI.L (GI.R (GI.C (GI.Rec a)))))
    Xaaba a -> GI.L (GI.L (GI.R (GI.L (GI.C (GI.Rec a)))))
    Xaabb a -> GI.L (GI.L (GI.R (GI.R (GI.C (GI.Rec a)))))
    Xabaa a -> GI.L (GI.R (GI.L (GI.L (GI.C (GI.Rec a)))))
    Xabab a -> GI.L (GI.R (GI.L (GI.R (GI.C (GI.Rec a)))))
    Xabba a -> GI.L (GI.R (GI.R (GI.L (GI.C (GI.Rec a)))))
    Xabbb a -> GI.L (GI.R (GI.R (GI.R (GI.C (GI.Rec a)))))
    Xbaaa a -> GI.R (GI.L (GI.L (GI.L (GI.C (GI.Rec a)))))
    Xbaab a -> GI.R (GI.L (GI.L (GI.R (GI.C (GI.Rec a)))))
    Xbaba a -> GI.R (GI.L (GI.R (GI.L (GI.C (GI.Rec a)))))
    Xbabb a -> GI.R (GI.L (GI.R (GI.R (GI.C (GI.Rec a)))))
    Xbbaa a -> GI.R (GI.R (GI.L (GI.L (GI.C (GI.Rec a)))))
    Xbbab a -> GI.R (GI.R (GI.L (GI.R (GI.C (GI.Rec a)))))
    Xbbba a -> GI.R (GI.R (GI.R (GI.L (GI.C (GI.Rec a)))))
    Xbbbb a -> GI.R (GI.R (GI.R (GI.R (GI.C (GI.Rec a)))))
  to = \case
    GI.L (GI.L (GI.L (GI.L (GI.C (GI.Rec a))))) -> Xaaaa a
    GI.L (GI.L (GI.L (GI.R (GI.C (GI.Rec a))))) -> Xaaab a
    GI.L (GI.L (GI.R (GI.L (GI.C (GI.Rec a))))) -> Xaaba a
    GI.L (GI.L (GI.R (GI.R (GI.C (GI.Rec a))))) -> Xaabb a
    GI.L (GI.R (GI.L (GI.L (GI.C (GI.Rec a))))) -> Xabaa a
    GI.L (GI.R (GI.L (GI.R (GI.C (GI.Rec a))))) -> Xabab a
    GI.L (GI.R (GI.R (GI.L (GI.C (GI.Rec a))))) -> Xabba a
    GI.L (GI.R (GI.R (GI.R (GI.C (GI.Rec a))))) -> Xabbb a
    GI.R (GI.L (GI.L (GI.L (GI.C (GI.Rec a))))) -> Xbaaa a
    GI.R (GI.L (GI.L (GI.R (GI.C (GI.Rec a))))) -> Xbaab a
    GI.R (GI.L (GI.R (GI.L (GI.C (GI.Rec a))))) -> Xbaba a
    GI.R (GI.L (GI.R (GI.R (GI.C (GI.Rec a))))) -> Xbabb a
    GI.R (GI.R (GI.L (GI.L (GI.C (GI.Rec a))))) -> Xbbaa a
    GI.R (GI.R (GI.L (GI.R (GI.C (GI.Rec a))))) -> Xbbab a
    GI.R (GI.R (GI.R (GI.L (GI.C (GI.Rec a))))) -> Xbbba a
    GI.R (GI.R (GI.R (GI.R (GI.C (GI.Rec a))))) -> Xbbbb a

-- Generic instances based on GI

instance Show (X t4 t3 t2 t1) where show = GI.gshowDefault
instance Eq (X t4 t3 t2 t1) where (==) = GI.geqDefault
instance Hashable (X t4 t3 t2 t1) where hashWithSalt = GI.ghashWithSalt
instance NFData (X t4 t3 t2 t1) where rnf = GI.grnf
instance Ae.ToJSON (X t4 t3 t2 t1) where toJSON = GI.gtoJSON

instance Ae.FromJSON (X 'T4a 'T3a 'T2a 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3a 'T2a 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3a 'T2b 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3a 'T2b 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3b 'T2a 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3b 'T2a 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3b 'T2b 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4a 'T3b 'T2b 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3a 'T2a 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3a 'T2a 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3a 'T2b 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3a 'T2b 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3b 'T2a 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3b 'T2a 'T1b) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3b 'T2b 'T1a) where parseJSON = GI.gparseJSON
instance Ae.FromJSON (X 'T4b 'T3b 'T2b 'T1b) where parseJSON = GI.gparseJSON

instance By.Serial (X 'T4a 'T3a 'T2a 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3a 'T2a 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3a 'T2b 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3a 'T2b 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3b 'T2a 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3b 'T2a 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3b 'T2b 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4a 'T3b 'T2b 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3a 'T2a 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3a 'T2a 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3a 'T2b 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3a 'T2b 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3b 'T2a 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3b 'T2a 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3b 'T2b 'T1a) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
instance By.Serial (X 'T4b 'T3b 'T2b 'T1b) where { serialize = GI.gserialize; deserialize = GI.gdeserialize }
