{-# OPTIONS_GHC -fno-warn-redundant-constraints   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tensor.Tensor where

import           Data.List                    (group, intercalate)
import           Data.Proxy
import           Data.Singletons
import qualified Data.Singletons.Prelude      as N
import qualified Data.Singletons.Prelude.List as N
import           Data.Tensor.Index
import           Data.Tensor.Type
import qualified Data.Vector                  as V
import           GHC.Exts                     (IsList (..))
import           GHC.TypeLits

-----------------------
-- Tensor
-----------------------

-- | Definition of <https://en.wikipedia.org/wiki/Tensor Tensor>.
-- `s` means shape of tensor.
--
-- > identity :: Tensor '[3,3] Int
newtype Tensor (s :: [Nat]) n = Tensor { getValue :: Index -> Index -> n }

-- | <https://en.wikipedia.org/wiki/Scalarr_(mathematics) Scalar> is rank 0 of tensor
type Scalar n  = Tensor '[] n

-- | <https://en.wikipedia.org/wiki/Vector_(mathematics_and_physics) Vector> is rank 1 of tensor
type Vector s n = Tensor '[s] n

-- | <https://en.wikipedia.org/wiki/Matrix_(mathematics) Matrix> is rank 2 of tensor
type Matrix a b n = Tensor '[a,b] n

-- | Simple Tensor is rank `r` tensor, has `n^r` dimension in total.
--
-- > SimpleTensor 2 3 Int == Matrix 3 3 Int == Tensor '[3,3] Int
-- > SimpleTensor r 0 Int == Scalar Int
type SimpleTensor (r :: Nat) (dim :: Nat) n = N.If ((N.==) dim 0) (Scalar n) (Tensor (N.Replicate r dim) n)

type TensorRank (s :: [Nat]) = N.Length s

instance SingI s => Functor (Tensor s) where
  fmap f (Tensor t) = Tensor (\s i -> f (t s i))

instance SingI s => Applicative (Tensor s) where
  pure n = Tensor $ \_ _ -> n
  Tensor f <*> Tensor t = Tensor $ \s i -> f s i (t s i)

instance SingI s => Foldable (Tensor s) where
  foldMap f t = foldMap (f.(t !)) ([minBound..maxBound] :: [TensorIndex s])

instance (SingI s, Show n) => Show (Tensor s n) where
  show (Tensor f) = let s = natsVal (Proxy :: Proxy s) in go 0 [] s (f s)
    where
      {-# INLINE go #-}
      go :: Int -> [Int] -> [Int] -> (Index -> n) -> String
      go _ i []     fs = show $ fs (reverse i)
      go z i [n]    fs = g2 n z "," $ fmap (\x -> show (fs $ reverse (x:i))) [0..n-1]
      go z i (n:ns) fs = g2 n z ",\n" $ fmap (\x -> go (z+1) (x:i) ns fs) [0..n-1]
      {-# INLINE g2 #-}
      g2 n z sep xs = let x = g3 n z xs in "[" ++ intercalate sep x ++ "]"
      {-# INLINE g3 #-}
      g3 n z xs
        | z > 3 = take 3 xs ++ [ "..", last xs]
        | n > 9 = take 8 xs ++ [ "..", last xs]
        | otherwise = xs

-----------------------
-- Tensor as Num
-----------------------
instance (SingI s, Num n) => Num (Tensor s n) where
  (+) = zipWithTensor (+)
  (*) = zipWithTensor (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance (SingI s, Fractional n) => Fractional (Tensor s n) where
  fromRational = pure . fromRational
  (/) = zipWithTensor (/)

instance (SingI s, Floating n) => Floating (Tensor s n) where
  pi      = pure pi
  exp     = fmap exp
  log     = fmap log
  sqrt    = fmap sqrt
  logBase = error "undefined"
  sin     = fmap sin
  cos     = fmap cos
  tan     = fmap tan
  asin    = fmap asin
  acos    = fmap acos
  atan    = fmap atan
  sinh    = fmap sinh
  cosh    = fmap cosh
  tanh    = fmap tanh
  asinh   = fmap asinh
  acosh   = fmap acosh
  atanh   = fmap atanh


{-# INLINE generateTensor #-}
generateTensor :: SingI s => (Index -> n) -> Proxy s -> Tensor s n
generateTensor fn p =
  let s  = natsVal p
      ps = product s
  in if ps == 0 then pure (fn [0]) else Tensor $ \_ -> fn

{-# INLINE transformTensor #-}
transformTensor
  :: forall s s' n. SingI s
  => (([Int], [Int]) -> [Int] -> [Int])
  -> Tensor s  n
  -> Tensor s' n
transformTensor go (Tensor f) = let s = natsVal (Proxy :: Proxy s) in Tensor $ \s' i' -> f s (go (i',s') s)

-- | Clone tensor to a new `V.Vector` based tensor
clone :: SingI s => Tensor s n -> Tensor s n
clone t =
  let s = shape t
      v = V.generate (product s) (\i -> t ! toEnum i)
  in Tensor $ \_ i -> v V.! tiTovi s i

{-# INLINE zipWithTensor #-}
zipWithTensor :: SingI s => (n -> n -> n) -> Tensor s n -> Tensor s n -> Tensor s n
zipWithTensor f t1 t2 = generateTensor (\i -> f (t1 ! TensorIndex i) (t2 ! TensorIndex i)) Proxy

instance SingI s => IsList (Tensor s n) where
  type Item (Tensor s n) = n
  fromList v =
    let s = natsVal (Proxy :: Proxy s)
        l = product s
    in if l /= length v
      then error "length not match"
      else let vv = V.fromList v in Tensor $ \s' i -> vv V.! tiTovi s' i
  toList  t = let n = rank t - 1 in fmap (\i -> t ! toEnum i) [0..n]

-----------------------
-- Tensor Shape
-----------------------
-- | Shape of Tensor, is a list of integers, uniquely determine the shape of tensor.
shape :: forall s n. SingI s => Tensor s n -> [Int]
shape _ = natsVal (Proxy :: Proxy s)

-- | Rank of Tensor
rank :: SingI s => Tensor s n -> Int
rank = length . shape

-----------------------
-- Tensor Operation
-----------------------
-- | Get value from tensor by index
(!) :: SingI s => Tensor s n -> TensorIndex s -> n
(!) t (TensorIndex i) = getValue t (shape t) i

-- | Reshape a tensor to another tensor, with total dimensions are equal.
reshape :: (N.Product s ~ N.Product s', SingI s) => Tensor s n -> Tensor s' n
reshape = transformTensor go
  where
    {-# INLINE go #-}
    go (i',s') s = viToti s $ tiTovi s' i'

type Transpose (a :: [Nat]) = N.Reverse a

-- | <https://en.wikipedia.org/wiki/Transpose Transpose> tensor completely
--
-- > λ> a = [1..9] :: Tensor '[3,3] Int
-- > λ> a
-- > [[1,2,3],
-- > [4,5,6],
-- > [7,8,9]]
-- > λ> transpose a
-- > [[1,4,7],
-- > [2,5,8],
-- > [3,6,9]]
transpose :: SingI a => Tensor a n -> Tensor (Transpose a) n
transpose  = transformTensor go
  where
    {-# INLINE go #-}
    go (i',_) _ = reverse i'

-- | Unit tensor of shape s, if all the indices are equal then return 1, otherwise return 0.
identity :: forall s n . (SingI s, Num n) => Tensor s n
identity = generateTensor ((\i -> if i == 1 then 1 else 0) . length . group) Proxy

dyad'
  :: ( r ~ (N.++) s t
     , SingI s
     , SingI t
     , SingI r)
  => (n -> m -> o)
  -> Tensor s n
  -> Tensor t m
  -> Tensor r o
dyad' f t1 t2 =
  let l = rank t1
  in generateTensor (\i -> let (ti1,ti2) = splitAt l i in f (t1 ! TensorIndex ti1) (t2 ! TensorIndex ti2)) Proxy

-- | <https://en.wikipedia.org/wiki/Dyadics Dyadic Tensor>
--
-- > λ> a = [1..4] :: Tensor '[2,2] Int
-- > λ> a
-- > [[1,2],
-- > [3,4]]
-- > λ> :t a `dyad` a
-- > a `dyad` a :: Tensor '[2, 2, 2, 2] Int
-- > λ> a `dyad` a
-- > [[[[1,2],
-- > [3,4]],
-- > [[2,4],
-- > [6,8]]],
-- > [[[3,6],
-- > [9,12]],
-- > [[4,8],
-- > [12,16]]]]
dyad
  :: ( r ~ (N.++) s t
     , SingI s
     , SingI t
     , SingI r
     , Num n)
  => Tensor s n -> Tensor t n -> Tensor r n
dyad = dyad' (*)


type DotTensor s1 s2 = (N.++) (N.Init s1) (N.Tail s2)

-- | Tensor Product
--
-- > λ> a = [1..4] :: Tensor '[2,2] Int
-- > λ> a
-- > [[1,2],
-- > [3,4]]
-- > λ> a `dot` a
-- > [[7,10],
-- > [15,22]]
--
-- > dot a b == contraction (dyad a b) (rank a - 1, rank a)
--
-- For rank 2 tensor, it is just matrix product.
dot
  :: ( N.Last s ~ N.Head s'
     , SingI (DotTensor s s')
     , SingI s
     , SingI s'
     , Num n)
  => Tensor s n
  -> Tensor s' n
  -> Tensor (DotTensor s s') n
dot t1 t2 =
  let s1 = shape t1
      n  = last s1
      b  = length s1 - 1
  in generateTensor (\i ->
        let (ti1,ti2) = splitAt b i
        in sum $ fmap (\(x,y) -> (t1 ! TensorIndex x) * (t2 ! TensorIndex y)) [(ti1++[x],x:ti2)| x <- [0..n-1]]) Proxy


type ContractionCheck s x y = N.And '[(N.<) x y, (N.>=) x 0, (N.<) y (TensorRank s)]
type Contraction s x y = DropIndex (DropIndex s y) x
type family TensorDim (s :: [Nat]) (i :: Nat) :: Nat where
  TensorDim s i = (N.!!) s i
type DropIndex (s :: [Nat]) (i :: Nat) = (N.++) (N.Fst (N.SplitAt i s)) (N.Tail (N.Snd (N.SplitAt i s)))

-- | Contraction Tensor
--
-- > λ> a = [1..16] :: Tensor '[4,4] Int
-- > λ> a
-- > [[1,2,3,4],
-- > [5,6,7,8],
-- > [9,10,11,12],
-- > [13,14,15,16]]
-- > λ> contraction a (i0,i1)
-- > 34
--
-- In rank 2 tensor, contraction of tensor is just the <https://en.wikipedia.org/wiki/Trace_(linear_algebra) trace>.
contraction
  :: forall x y s s' n.
     ( ContractionCheck s x y ~ 'True
     , s' ~ Contraction s x y
     , TensorDim s x ~ TensorDim s y
     , KnownNat x
     , KnownNat y
     , SingI s
     , SingI s'
     , KnownNat  (TensorDim s x)
     , Num n)
  => Tensor s  n
  -> (Proxy x, Proxy y)
  -> Tensor s' n
contraction t@(Tensor f) (px, py) =
  let x  = toNat px
      y  = toNat py
      n  = toNat (Proxy :: Proxy (TensorDim s x))
      s  = shape t
  in generateTensor (go x (y-x-1) n (f s) ) Proxy
  where
    {-# INLINE go #-}
    go a b n fs i =
      let (r1,rt) = splitAt a i
          (r3,r4) = splitAt b rt
      in sum $ fmap fs [r1 ++ (j:r3) ++ (j:r4) | j <- [0..n-1]]

type CheckDim dim s = N.And '[(N.>=) dim 0, (N.<) dim (N.Length s)]

type CheckSelect dim i s = N.And '[ CheckDim dim s , (N.>=) i 0, (N.<) i ((N.!!) s dim) ]

type Select i s = (N.++) (N.Take i s) (N.Tail (N.Drop i s))

-- | Select `i` indexing of tensor
--
-- > λ> a = identity :: Tensor '[4,4] Int
-- > λ> select a (i0,i0)
-- > [1,0,0,0]
-- > λ> select a (i0,i1)
-- > [0,1,0,0]
select
  :: ( CheckSelect dim i s ~ 'True
     , s' ~ Select dim s
     , SingI s
     , KnownNat dim
     , KnownNat i)
  => Tensor s n
  -> (Proxy dim, Proxy i)
  -> Tensor s' n
select t (pd, pid) =
  let dim = toNat pd
      ind = toNat pid
  in transformTensor (go dim ind) t
  where
    {-# INLINE go #-}
    go d i (i',_) _ = let (a,b) = splitAt d i' in a ++ (i:b)

type CheckSlice dim from to s = N.And '[ CheckDim dim s, CheckSelect dim from s, (N.<) from to , (N.<=) to ((N.!!) s dim)]
type Slice dim from to s = N.Concat '[N.Take dim s, '[(N.-) to from] , N.Tail (N.Drop dim s)]

-- | Slice tensor
--
-- > λ> a = identity :: Tensor '[4,4] Int
-- > λ> a
-- > [[1,0,0,0],
-- > [0,1,0,0],
-- > [0,0,1,0],
-- > [0,0,0,1]]
-- > λ> slice a (i0,(i1,i3))
-- > [[0,1,0,0],
-- > [0,0,1,0]]
-- > λ> slice a (i1,(i1,i3))
-- > [[0,0],
-- > [1,0],
-- > [0,1],
-- > [0,0]]
slice
  :: ( CheckSlice dim from to s ~ 'True
     , s' ~ Slice dim from to s
     , KnownNat dim
     , KnownNat from
     , KnownNat ((N.-) to from)
     , SingI s)
  => Tensor s n
  -> (Proxy dim, (Proxy from, Proxy to))
  -> Tensor s' n
slice t (pd, (pa,_)) =
  let d = toNat pd
      a = toNat pa
  in transformTensor (\(i',_) _ -> let (x,y:ys) = splitAt d i' in x ++ (y+a:ys)) t

type CheckExpand s s' = N.And '[(N.==) (TensorRank s) (TensorRank s')]

-- | Expand tensor
--
-- > λ> a = identity :: Tensor '[2,2] Int
-- > λ> a
-- > [[1,0],
-- > [0,1]]
-- > λ> expand a :: Tensor '[4,4] Int
-- > [[1,0,1,0],
-- > [0,1,0,1],
-- > [1,0,1,0],
-- > [0,1,0,1]]
expand
  :: (TensorRank s ~ TensorRank s'
     , SingI s)
  => Tensor s n
  -> Tensor s' n
expand = transformTensor go
  where
    {-# INLINE go #-}
    go (i',_) = zipWith mod i'

-- | Convert tensor to untyped function, for internal usage.
runTensor :: SingI s => Tensor s n -> [Int] -> n
runTensor t@(Tensor f) = f (shape t)
