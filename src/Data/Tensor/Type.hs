module Data.Tensor.Type where

import           Data.List                    (foldl')
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           GHC.TypeLits
import           Unsafe.Coerce

type Shape = [Int]
type Index = [Int]

toNat :: KnownNat s => Proxy s -> Int
toNat = unsafeCoerce . natVal

natsVal :: forall proxy (s::[Nat]). SingI s => proxy s -> Index
natsVal _ = case (sing :: Sing s) of
  SNil         -> []
  (SCons x xs) -> unsafeCoerce <$> (fromSing x: fromSing xs)

viToti :: Shape -> Int -> Index
viToti s i = go i [] (reverse s)
  where
    {-# INLINE go #-}
    go _ xs [] = xs
    go r xs (si:ss) = let (r',x) = divMod r si in go r' (x:xs) ss

tiTovi :: Shape -> Index -> Int
tiTovi s i = foldl' (\b (n,ind) -> b * n + ind) 0 $ zipWith (,) s i 

mult :: (Eq a, Num a) => a -> a -> a
mult a b
  | a == 0 = 0
  | b == 0 = 0
  | otherwise = a * b
-----------------------
-- Tensor Type Index
-----------------------
i0  = Proxy :: Proxy 0
i1  = Proxy :: Proxy 1
i2  = Proxy :: Proxy 2
i3  = Proxy :: Proxy 3
i4  = Proxy :: Proxy 4
i5  = Proxy :: Proxy 5
i6  = Proxy :: Proxy 6
i7  = Proxy :: Proxy 7
i8  = Proxy :: Proxy 8
i9  = Proxy :: Proxy 9
