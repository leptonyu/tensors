module Data.Tensor.Space where

import           Data.Proxy
import qualified Data.Singletons.Prelude      as N
import qualified Data.Singletons.Prelude.List as N
import           Data.Tensor.Tensor
import           Data.Tensor.Type
import qualified Data.Vector                  as V
import           GHC.TypeLits

-- | Return evenly spaced numbers over a specified interval.
--
-- > λ> linspace 1 10 :: Tensor '[10] Float
-- > [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,..,10.0]
linspace :: forall s n. (KnownNat s, Fractional n) => n -> n -> Vector s n
linspace = space (\d s i -> s + d * fromInteger i) (\s e c -> (e - s) / fromInteger (c - 1))

-- | Return numbers spaced evenly on a log scale (a geometric progression).
--
-- > λ> geospace 1 10 :: Vector 10 Float
-- > [1.0,1.2915497,1.6681006,2.1544347,2.7825596,3.593814,4.641589,5.994843,..,10.000001]
geospace :: forall s n. (KnownNat s, Floating n, Eq n) => n -> n -> Vector s n
geospace start end
  | start == 0 = error "divided by zero"
  | otherwise  = space (\d s i -> s * (d ** fromInteger i)) (\s e c -> (e / s) ** (1 / fromInteger (c - 1))) start end

-- | Return numbers spaced evenly on a log scale.
--
-- In linear space, the sequence starts at base ** start (base to the power of start) and ends with base ** stop
--
-- > λ> logspace 10 2 3 :: Vector 4 Float
-- > [100.0,215.44347,464.15887,1000.0]
logspace :: forall s n. (KnownNat s, Floating n, Eq n) => n -> n -> n -> Vector s n
logspace base s e = geospace (base ** s) (base ** e)

space :: forall s n. (KnownNat s) => (n -> n -> Integer -> n) -> (n -> n -> Integer -> n) -> n -> n -> Vector s n
space = go (natVal (Proxy :: Proxy s))
  where
    go count g f start end
      | count <= 1 = pure start
      | otherwise =
        let d  = f start end count
            v  = V.generate (fromInteger count) (g d start . toInteger)
        in Tensor $ \s i -> v V.! tiTovi s i


type CheckGrid i a s = N.And '[ (N.>=) i 0, (N.<) i (N.Length s)]
-- | Grid
--
-- > λ> a = [1..2] :: Vector 2 Int
-- > λ> a
-- > [1,2]
-- > λ> grid i0 a :: Tensor '[2,2,2] Int
-- > [[[1,1],
-- > [1,1]],
-- > [[2,2],
-- > [2,2]]]
-- > λ> grid i1 a :: Tensor '[2,2,2] Int
-- > [[[1,1],
-- > [2,2]],
-- > [[1,1],
-- > [2,2]]]
-- > λ> grid i2 a :: Tensor '[2,2,2] Int
-- > [[[1,2],
-- > [1,2]],
-- > [[1,2],
-- > [1,2]]]
grid
  :: (CheckDim i s ~ 'True
    , KnownNat i
    , KnownNat (TensorDim s i))
  => Proxy i
  -> Vector (TensorDim s i) n
  -> Tensor s n
grid p v@(Tensor t) =
  let i = toNat p
      a = shape v
  in Tensor $ \_ ind -> t a [ind !! i]
