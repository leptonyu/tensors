module Data.Tensor.Index where

import           Data.Proxy
import           Data.Singletons
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
