{-# LANGUAGE UndecidableInstances #-}
module Data.Tensor.Index where

import           Data.Proxy
import           Data.Reflection
import           Data.Singletons
import qualified Data.Singletons.Prelude.List as N
import           Data.Tensor.Type
import           GHC.Exts
import           GHC.TypeLits

-- | Tensor Index, used to locate each point of tensor
newtype TensorIndex (shape :: [Nat]) = TensorIndex Index deriving (Eq,Show,Ord)

instance forall s. SingI s => Bounded (TensorIndex s) where
  minBound = toEnum 0
  maxBound = let s = natsVal (Proxy :: Proxy s) in  toEnum (product s - 1)

instance forall s. SingI s =>  Enum (TensorIndex s) where
  toEnum i   = let s = natsVal (Proxy :: Proxy s) in TensorIndex $ viToti s i
  fromEnum (TensorIndex i) = let s = natsVal (Proxy :: Proxy s) in tiTovi s i

instance forall s. SingI s => IsList (TensorIndex s) where
  type Item (TensorIndex s) = Int
  fromList v =
    let s = natsVal (Proxy :: Proxy s)
    in if length v /= length s then error "length not match"
        else if or (zipWith (\i n-> i <0 || i >= n) v s) then error "index overflow"
          else TensorIndex v
  toList (TensorIndex v) = v


-- | Tensor rank.
type TensorRank (s :: [Nat]) = N.Length s

-- type TensorRankConstraint s i = N.And '[ (N.>=) i 0, (N.<) i (TensorRank s)]
data TensorRankIndex (shape :: [Nat]) = forall (i :: Nat). KnownNat i => TensorRankIndex (Proxy i)

instance SingI s => Show (TensorRankIndex s) where
  show = show . fromEnum

instance forall s. (SingI s, KnownNat (TensorRank s - 1)) => Bounded (TensorRankIndex s) where
  minBound = TensorRankIndex i0
  maxBound = TensorRankIndex (Proxy :: Proxy (TensorRank s - 1))

instance forall (s::[Nat]). (SingI s) => Enum (TensorRankIndex s) where
  toEnum i =  reifyNat (toInteger i) TensorRankIndex
  fromEnum (TensorRankIndex p) = fromInteger $ natVal p
