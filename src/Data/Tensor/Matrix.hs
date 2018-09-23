{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Data.Tensor.Matrix where

import           Data.List          (foldl')
import           Data.Proxy
import           Data.Ratio
import           Data.Tensor.Tensor
import           Data.Tensor.Type
import           GHC.TypeLits

type SimpleMatrix a n = Matrix a a n

-- | <https://en.wikipedia.org/wiki/Matrix_multiplication Matrix multiplication>
dotM :: (KnownNat a, Num n, Eq n) => SimpleMatrix a n -> SimpleMatrix a n -> SimpleMatrix a n
dotM = dot

diag :: SimpleMatrix a n -> Vector a n
diag (Tensor t) = Tensor $ \[s] [i] -> t [s,s] [i,i]

-- | <https://en.wikipedia.org/wiki/Trace_(linear_algebra) Matrix trace>
trace :: (KnownNat a, Num n) => SimpleMatrix a n -> n
trace t = let (Tensor f) = contraction (i0,i1) t in f [] []

-- | <https://en.wikipedia.org/wiki/LU_decomposition LU decomposition> of n x n matrix
--
-- > λ> a = [1,2,3,2,5,7,3,5,3]:: Tensor '[3,3] Int
-- > λ> (l,u,p) = lu a
-- > λ> l
-- > [[1 % 1,0 % 1,0 % 1],
-- > [2 % 1,1 % 1,0 % 1],
-- > [3 % 1,(-1) % 1,1 % 1]]
-- > λ> u
-- > [[1 % 1,2 % 1,3 % 1],
-- > [0 % 1,1 % 1,1 % 1],
-- > [0 % 1,0 % 1,(-5) % 1]]
-- > λ> p
-- > [[1,0,0],
-- > [0,1,0],
-- > [0,0,1]]
lu :: forall a n . (KnownNat a, Integral n)
   => SimpleMatrix a n
   -> (SimpleMatrix a (Ratio n), SimpleMatrix a (Ratio n), SimpleMatrix a n)
lu t =
  let a  = toNat (Proxy :: Proxy a)
      (l,u,p,_) = foldl' go (identity, fmap (% 1) t, identity, [a,a]) ([0..a-1] :: [Int])
  in (l, u, p)
  where
    {-# INLINE go #-}
    go (l,u@(Tensor f),p@(Tensor fp),s) i =
      let ii = f s [i,i]
      in if ii == 0 then
          let is = filter (\j -> f s [i,j] /= 0) [i+1..head s-1]
          in if null is then (l,u,p,s)
            else let j  = head is
                     u' = swap i j (f  s)
                     p' = swap i j (fp s)
                 in go (l,u',p',s) i
        else
          let ij = denominator ii % numerator ii
              li = Tensor $ \_ i' -> ij * gi i (f s) i'
              lj = Tensor $ \_ i' -> ij * gj i (f s) i'
          in (l `dotM` lj, li `dotM` u, p, s)
    {-# INLINE gi #-}
    gi a fs [x,y]
      | x > a && y == a = - (fs [x,y])
      | x == y = fs [a,a]
      | otherwise = 0
    {-# INLINE gj #-}
    gj a fs [x,y]
      | x > a && y == a = fs [x,y]
      | x == y = fs [a,a]
      | otherwise = 0
    {-# INLINE swap #-}
    swap a b g = Tensor $ \_ [x,y] -> if y == a then g [x,b] else if y == b then g [x,a] else g [x,y]

-- | determiant using `lu` decomposition
det' :: forall a n . (KnownNat a, Integral n) => SimpleMatrix a n -> n
det' t =
  let (l,u,p) = lu t
      s = shape t
      r = head s
      v = go s r u
      w = det p
  in if v == 0 then 0 else go s r l * v * w
  where
    {-# INLINE go #-}
    go s' r' (Tensor f) = let fs = f s' in numerator $ product $ fmap (\i -> fs [i,i]) ([0..r' - 1] :: [Int])

-- | <https://en.wikipedia.org/wiki/Determinant Determinant> of n x n matrix
--
-- > λ> a = [2,0,1,3,0,0,2,2,1,2,1,34,3,2,34,4] :: Tensor '[4,4] Int
-- > λ> a
-- > [[2,0,1,3],
-- > [0,0,2,2],
-- > [1,2,1,34],
-- > [3,2,34,4]]
-- > λ> det a
-- > 520
--
-- This implementation is not so fast, it can calculate 8 x 8 in 1 second with all the num none zero on my computer.
-- It should be faster if more zero in the matrix.
det :: forall a n. (KnownNat a, Num n, Eq n) => SimpleMatrix a n -> n
det = let n = toNat (Proxy :: Proxy a) in go n . runTensor
  where
    {-# INLINE go #-}
    go 1 f = f [0,0]
    go n f = sum $ fmap (g2 f n) ([0.. n-1] :: [Int])
    {-# INLINE g2 #-}
    g2 f n i =
      let f' [x,y] = if y >= i then f [x+1,y +1] else f [x+1,y]
      in f [0,i] `mult` (if even i then go (n-1) f' else - (go (n-1) f'))
