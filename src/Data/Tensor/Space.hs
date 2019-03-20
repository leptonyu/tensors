module Data.Tensor.Space where

import           Data.Proxy
import           Data.Tensor.Tensor
import           Data.Tensor.Type
import qualified Data.Vector        as V
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
  :: (CheckDimension i s ~ 'True
    , KnownNat i
    , KnownNat (Dimension s i))
  => Proxy i
  -> Vector (Dimension s i) n
  -> Tensor s n
grid p v@(Tensor t) =
  let i = toNat p
      a = shape v
  in Tensor $ \_ ind -> t a [ind !! i]

-- | Generate 2 dimension grid coordinates by two vectors.
meshgrid2 :: (s ~ '[a,b], KnownNat a, KnownNat b) => Vector a n -> Vector b n -> [Tensor s n]
meshgrid2 a b = [grid i0 a, grid i1 b]

-- | Generate 3 dimension grid coordinates by three vectors.
meshgrid3 :: (s ~ '[a,b,c], KnownNat a, KnownNat b, KnownNat c) => Vector a n -> Vector b n -> Vector c n -> [Tensor s n]
meshgrid3 a b c = [grid i0 a, grid i1 b, grid i2 c]

-- | Generate 4 dimension grid coordinates by four vectors.
meshgrid4
  :: (s ~ '[a,b,c,d], KnownNat a, KnownNat b, KnownNat c, KnownNat d)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> [Tensor s n]
meshgrid4 a b c d= [grid i0 a, grid i1 b, grid i2 c, grid i3 d]

-- | Generate 5 dimension grid coordinates by five vectors.
meshgrid5
  :: (s ~ '[a,b,c,d,e], KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> Vector e n -> [Tensor s n]
meshgrid5 a b c d e = [grid i0 a, grid i1 b, grid i2 c, grid i3 d, grid i4 e]

-- | Generate 6 dimension grid coordinates by six vectors.
meshgrid6
  :: (s ~ '[a,b,c,d,e,f], KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e, KnownNat f)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> Vector e n -> Vector f n -> [Tensor s n]
meshgrid6 a b c d e f = [grid i0 a, grid i1 b, grid i2 c, grid i3 d, grid i4 e, grid i5 f]

-- | Generate 7 dimension grid coordinates by seven vectors.
meshgrid7
  :: (s ~ '[a,b,c,d,e,f,g], KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e, KnownNat f, KnownNat g)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> Vector e n -> Vector f n -> Vector g n -> [Tensor s n]
meshgrid7 a b c d e f g = [grid i0 a, grid i1 b, grid i2 c, grid i3 d, grid i4 e, grid i5 f, grid i6 g]

-- | Generate 8 dimension grid coordinates by eight vectors.
meshgrid8
  :: (s ~ '[a,b,c,d,e,f,g,h], KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e, KnownNat f, KnownNat g, KnownNat h)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> Vector e n -> Vector f n -> Vector g n -> Vector h n -> [Tensor s n]
meshgrid8 a b c d e f g h = [grid i0 a, grid i1 b, grid i2 c, grid i3 d, grid i4 e, grid i5 f, grid i6 g, grid i7 h]

-- | Generate 9 dimension grid coordinates by nine vectors.
meshgrid9
  :: (s ~ '[a,b,c,d,e,f,g,h,i], KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e, KnownNat f, KnownNat g, KnownNat h, KnownNat i)
  => Vector a n -> Vector b n -> Vector c n -> Vector d n -> Vector e n -> Vector f n -> Vector g n -> Vector h n -> Vector i n -> [Tensor s n]
meshgrid9 a b c d e f g h i = [grid i0 a, grid i1 b, grid i2 c, grid i3 d, grid i4 e, grid i5 f, grid i6 g, grid i7 h, grid i8 i]
