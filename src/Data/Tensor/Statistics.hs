module Data.Tensor.Statistics where

import           Data.Tensor.Tensor
import           Data.Tensor.Type

-- | Average of tensor
--
-- > λ> average (identity :: Tensor '[3,3] Float)
-- > 0.33333334
average :: forall s n. (HasShape s, Fractional n) => Tensor s n -> n
average t =
  let v = sum t
      s = fromInteger $ toInteger $ product $ shape t
  in v / s

-- | Variance of tensor
--
-- > λ> var ([1,2,3,4] :: Vector 4 Double )
-- > 1.25
var :: forall s n. (HasShape s, Fractional n) => Tensor s n -> n
var t =
  let m = average t
      r = fmap (\v -> let x = v - m in x * x) t
  in average r

-- | Standard Deviation of tensor
--
-- > λ> std ([1,2,3,4] :: Vector 4 Double )
-- > 1.118033988749895
std :: forall s n. (HasShape s, Floating n) => Tensor s n -> n
std = sqrt . var

