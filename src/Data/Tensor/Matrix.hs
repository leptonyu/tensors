{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.Tensor.Matrix where

import           Data.List          (foldl')
import           Data.Proxy
import           Data.Tensor.Tensor
import           Data.Tensor.Type
import           GHC.TypeLits

type SimpleMatrix a n = Matrix a a n

dotM :: (KnownNat a, Num n) => SimpleMatrix a n -> SimpleMatrix a n -> SimpleMatrix a n
dotM = dot

trace :: (KnownNat a, Num n) => SimpleMatrix a n -> n
trace t = let (Tensor f) = contraction t (i0,i1) in f [] []

-- | <https://en.wikipedia.org/wiki/LU_decomposition LU decomposition> of n x n matrix
--
-- > λ> a = [1,2,3,2,5,7,3,5,3]:: Tensor '[3,3] Int
-- > λ> (l,u,m) = lu a
-- > λ> l
-- > [[1,0,0],
-- > [2,1,0],
-- > [3,-1,1]]
-- > λ> u
-- > [[1,2,3],
-- > [0,1,1],
-- > [0,0,-5]]
-- > λ> m
-- > 1
lu :: forall a n . (KnownNat a, Integral n) => SimpleMatrix a n -> (SimpleMatrix a n, SimpleMatrix a n, n)
lu t =
  let a  = toNat (Proxy :: Proxy a)
      (l,u,_,m) = foldl' go (identity, t, [a,a], 1) ([0..a-1] :: [Int])
      p  = minimum (fmap (abs.(`gcd` m)) l)  `min` minimum (fmap (abs.(`gcd` m)) u)
      g  = (`div` (p * signum m))
  in (fmap g l,fmap g u, g m)
  where
    go :: (SimpleMatrix a n, SimpleMatrix a n, [Int], n) -> Int -> (SimpleMatrix a n, SimpleMatrix a n,[Int],n)
    go (l,u@(Tensor f),s,m) i =
      let li = Tensor $ \_ -> gi i (f s)
          lj = Tensor $ \_ -> gj i (f s)
      in (l `dotM` lj, li `dotM` u, s, m * f s [i,i])
    gi a fs [x,y]
      | x > a && y == a = - (fs [x,y])
      | x == y = fs [a,a]
      | otherwise = 0
    gj a fs [x,y]
      | x > a && y == a = fs [x,y]
      | x == y = fs [a,a]
      | otherwise = 0

det' :: forall a n . (KnownNat a, Integral n) => SimpleMatrix a n -> n
det' t =
  let (l,u,m) = lu t
      s = shape t
      r = length s
  in (go s r l * go s r u) `div` (m ^ (r+1))
  where
    go s' r' (Tensor f) = let fs = f s' in product $ fmap (\i -> fs [i,i]) ([0..r' - 1] :: [Int])

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
    go :: Int -> ([Int] -> n) -> n
    go 1 f = f [0,0]
    go n f = sum $ zipWith (g2 f n) ([0.. n-1] :: [Int]) (cycle [1, -1])
    {-# INLINE g2 #-}
    g2 f n i sign = case f [0,i] of
      0 -> 0
      v -> let f' [x,y] = if y >= i then f [x+1,y +1] else f [x+1,y] in  sign * v * go (n-1) f'
