{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Tensor.Type where

import           Data.List      (foldl')
import           Data.Proxy
import           Data.Type.Bool hiding (If)
import           GHC.Exts
import           GHC.TypeLits
import qualified GHC.TypeLits   as L
import           Unsafe.Coerce

type Shape = [Int]
type Index = [Int]

newtype SShape (shape :: [Nat]) = SShape { unShape :: Shape } deriving Show

class HasShape s where
  toShape :: SShape s
  toRank  :: Proxy s -> Int
  toRank _ = length $ unShape (toShape :: SShape s)
  toSize  :: Proxy s -> Int
  toSize _ = product $ unShape (toShape :: SShape s)

instance HasShape '[] where
  toShape = SShape []

instance (KnownNat n, HasShape s) => HasShape (n:s) where
  toShape = SShape $ fromInteger (natVal (Proxy :: Proxy n)) : unShape (toShape :: SShape s)

toNat :: KnownNat n => Proxy n -> Int
toNat = unsafeCoerce . natVal

viToti :: Shape -> Int -> Index
viToti s i = snd $ foldl' (\(r,xs) si -> let (r',x) = divMod r si in (r', x:xs)) (i,[]) (reverse s)

tiTovi :: Shape -> Index -> Int
tiTovi s i = foldl' (\b (n,ind) -> b * n + ind) 0 $ zipWith (,) s i

-- | Tensor Index, used to locate each point of tensor
newtype TensorIndex (shape :: [Nat]) = TensorIndex Index deriving (Eq,Show,Ord)

instance HasShape s => IsList (TensorIndex s) where
  type Item (TensorIndex s) = Int
  fromList v =
    let s = unShape (toShape :: SShape s)
    in if length v /= length s then error "length not match"
        else if or (zipWith (\i n-> i <0 || i >= n) v s) then error "index overflow"
          else TensorIndex v
  toList (TensorIndex v) = v

-- | Tensor rank.
type family TensorRank (s :: [Nat]) :: Nat where
  TensorRank '[] = 0
  TensorRank (_:s) = TensorRank s + 1

-- | Tensor size.
type family TensorSize (s :: [Nat]) :: Nat where
  TensorSize '[] = 1
  TensorSize (n:s) = n L.* (TensorRank s)

type family Reverse (a :: [k]) (b :: [k]) :: [k] where
  Reverse '[]    b = b
  Reverse (a:as) b = Reverse as (a:b)

type family If (b :: Bool) c d where
  If 'True  c d = c
  If 'False c d = d

type family Replicate (a :: k) (dim :: Nat) :: [k] where
  Replicate a 0 = '[]
  Replicate a n = a : Replicate a n

type family Dimension (s :: [Nat]) (i :: Nat) :: Nat where
  Dimension (s:_) 0 = s
  Dimension (_:s) n = Dimension s (n-1)
  Dimension _ _     = TypeError ('Text "Index overflow")

type CheckDimension dim s = IsIndex dim (TensorRank s)
type CheckIndices i j s = IsIndices i j (TensorRank s) ~ 'True

type IsIndex i n = (0 <=? i) && (i + 1 <=? n)
type IsIndices i j n = (0 <=? i) && (i + 1 <=? j) && (j + 1 <=? n)

type family Take (n :: Nat) (a :: [k]) :: [k] where
  Take 0 _ = '[]
  Take n (x:xs) = x : Take (n-1) xs

type family Drop (n :: Nat) (a :: [k]) :: [k] where
  Drop 0 xs = xs
  Drop n (_:xs) = Take (n-1) xs

type family Tail (a :: [k]) :: [k] where
  Tail '[] = TypeError ('Text "No tail")
  Tail (_:xs) = xs

type family Init (a :: [k]) :: [k] where
  Init '[] = TypeError ('Text "No init")
  Init '[_] = '[]
  Init (x:xs) = x : Init xs

type family Head (a :: [k]) :: k where
  Head '[] = TypeError ('Text "No head")
  Head (x:_) = x

type family Last (a :: [k]) :: k where
  Last '[] = TypeError ('Text "No last")
  Last '[x] = x
  Last (_:xs) = Last xs

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ b = b
  (a:as) ++ b = a : (as ++ b)

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
