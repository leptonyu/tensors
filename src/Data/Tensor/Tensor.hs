{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tensor.Tensor where

import           Control.DeepSeq
import           Data.List        (intercalate)
import           Data.Proxy
import           Data.Tensor.Type
import           Data.Type.Bool   hiding (If)
import qualified Data.Vector      as V
import           GHC.Exts         (IsList (..))
import           GHC.TypeLits

-----------------------
-- Tensor
-----------------------

-- | Definition of <https://en.wikipedia.org/wiki/Tensor Tensor>.
-- `s` means shape of tensor.
--
-- > identity :: Tensor '[3,3] Int
newtype Tensor (s :: [Nat]) n = Tensor { getValue :: Shape -> Index -> n }

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
type SimpleTensor (r :: Nat) (dim :: Nat) n = Tensor (Replicate r dim) n

instance Functor (Tensor s) where
  fmap f t = Tensor $ \s i -> f (getValue t s i)

instance Applicative (Tensor s) where
  pure n = Tensor $ \_ _ -> n
  f <*> t = Tensor $ \s i -> getValue f s i (getValue t s i)

instance (HasShape s, Eq n) => Eq (Tensor s n) where
  f == t = and $ (==) <$> f <*> t

instance HasShape s => Foldable (Tensor s) where
  foldr f b t =
    let s = shape t
        r = toSize (Proxy :: Proxy s)
    in foldr (f . gx t s) b ([0..r-1] :: [Int])

instance (HasShape s, NFData a) => NFData (Tensor s a) where
  rnf = foldr (\_ -> rnf) () 

instance (HasShape s, Show n) => Show (Tensor s n) where
  show (Tensor f) = let s = unShape (toShape :: SShape s) in go 0 [] s (f s)
    where
      {-# INLINE go #-}
      go :: Int -> [Int] -> [Int] -> (Index -> n) -> String
      go _ i []     fs = show $ fs (reverse i)
      go z i [n]    fs = g2 n z "," $ fmap (\x -> show (fs $ reverse (x:i))) [0..n-1]
      go z i (n:ns) fs = g2 n z ",\n" $ fmap (\x -> go (z+1) (x:i) ns fs) [0..n-1]
      {-# INLINE g2 #-}
      g2 n z sep xs = let x = g3 n z xs in "[" ++ intercalate sep x ++ "]"
      {-# INLINE g3 #-}
      g3 n _ xs
        | n > 9 = take 8 xs ++ [ "..", last xs]
        | otherwise = xs

-----------------------
-- Tensor as Num
-----------------------
instance (HasShape s, Num n) => Num (Tensor s n) where
  (+) = zipWithTensor (+)
  (*) = zipWithTensor (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance (HasShape s, Fractional n) => Fractional (Tensor s n) where
  fromRational = pure . fromRational
  (/) = zipWithTensor (/)

instance (HasShape s, Floating n) => Floating (Tensor s n) where
  pi      = pure pi
  exp     = fmap exp
  log     = fmap log
  sqrt    = fmap sqrt
  logBase a b = logBase <$> a <*> b
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
generateTensor :: forall s n. HasShape s => (Index -> n) -> Tensor s n
generateTensor fn = case toSize (Proxy :: Proxy s) of
  0 -> pure (fn [])
  _ -> Tensor (const fn)

{-# INLINE transformTensor #-}
transformTensor
  :: forall s s' n. HasShape s
  => (Shape -> (Shape, Index) -> Index)
  -> Tensor s  n
  -> Tensor s' n
transformTensor go (Tensor fo) =
  let s = unShape (toShape :: SShape s)
      {-# INLINE g #-}
      g = curry $ fo s . go s
  in Tensor g

-- | Clone tensor to a new `V.Vector` based tensor
clone :: HasShape s => Tensor s n -> Tensor s n
clone t =
  let s = shape t
      v = V.generate (product s) (gx t s)
  in Tensor $ \_ i -> v V.! tiTovi s i

{-# INLINE zipWithTensor #-}
zipWithTensor :: HasShape s => (n -> n -> n) -> Tensor s n -> Tensor s n -> Tensor s n
zipWithTensor f t1 t2 =
  let s1 = shape t1
      s2 = shape t2
  in generateTensor (\i -> f (getValue t1 s1 i) (getValue t2 s2 i))

instance HasShape s => IsList (Tensor s n) where
  type Item (Tensor s n) = n
  fromList v =
    let s = unShape (toShape :: SShape s)
        l = product s
    in if l /= length v
      then error "length not match"
      else let vv = V.fromList v in Tensor $ \s' i -> vv V.! tiTovi s' i
  toList  t =
    let s = unShape (toShape :: SShape s)
        l = product s
    in fmap (gx t s) [0..pred l]

-----------------------
-- Tensor Shape
-----------------------
-- | Shape of Tensor, is a list of integers, uniquely determine the shape of tensor.
shape :: forall s n. HasShape s => Tensor s n -> [Int]
shape _ = unShape (toShape :: SShape s)

-- | Rank of Tensor
rank :: forall s n. HasShape s => Tensor s n -> Int
rank _ = toRank (Proxy :: Proxy s)

-----------------------
-- Tensor Operation
-----------------------
-- | Get value from tensor by index
(!) :: HasShape s => Tensor s n -> TensorIndex s -> n
(!) t (TensorIndex i) = getValue t (shape t) i

gx :: HasShape s => Tensor s n -> Shape -> Int -> n
gx (Tensor t) s i = t s (viToti s i)

-- | Reshape a tensor to another tensor, with total dimensions are equal.
reshape :: (TensorSize s ~ TensorSize s', HasShape s) => Tensor s n -> Tensor s' n
reshape = transformTensor go
  where
    {-# INLINE go #-}
    go s (s',i') = viToti s $ tiTovi s' i'

type Transpose (a :: [Nat]) = Reverse a '[]

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
transpose :: HasShape a => Tensor a n -> Tensor (Transpose a) n
transpose  = transformTensor go
  where
    {-# INLINE go #-}
    go _ (_, i') = reverse i'

type Swapaxes i j s = Take i s ++ (Dimension s j : (Drop i (Take j s))) ++ (Dimension s j : (Tail (Drop j s)))

-- | Swapaxes any rank
--
-- > λ> a = [1..24] :: Tensor '[2,3,4] Int
-- > λ> a
-- > [[[1,2,3,4],
-- > [5,6,7,8],
-- > [9,10,11,12]],
-- > [[13,14,15,16],
-- > [17,18,19,20],
-- > [21,22,23,24]]]
-- > λ> swapaxes i0 i1 a
-- > [[[1,2,3,4],
-- > [13,14,15,16]],
-- > [[5,6,7,8],
-- > [17,18,19,20]],
-- > [[9,10,11,12],
-- > [21,22,23,24]]]
-- > λ> :t swapaxes i0 i1 a
-- > swapaxes i0 i1 a :: Tensor '[3, 2, 4] Int
-- > λ> :t swapaxes i1 i2 a
-- > swapaxes i1 i2 a :: Tensor '[2, 4, 3] Int
--
-- In rank 2 tensor, `swapaxes` is just `transpose`
--
-- > transpose == swapaxes i0 i1
swapaxes
  :: (CheckIndices i j s
    , HasShape s
    , KnownNat i
    , KnownNat j)
  => Proxy i
  -> Proxy j
  -> Tensor s n
  -> Tensor (Swapaxes i j s) n
swapaxes px pj =
  let i = toNat px
      j = toNat pj
      go _ (_,s) = take i s ++ [s !! j] ++ tail (drop i (take j s)) ++ [s!!i] ++ tail (drop j s)
  in transformTensor go

-- | Unit tensor of shape s, if all the indices are equal then return 1, otherwise return 0.
identity :: forall s n . (HasShape s, Num n) => Tensor s n
identity = generateTensor go
  where
    go []  = 0
    go [_] = 1
    go (a:b:cs)
      | a /= b = 0
      | otherwise = go (b:cs)

dyad'
  :: ( r ~ (s ++ t)
     , HasShape s
     , HasShape t
     , HasShape r)
  => (n -> m -> o)
  -> Tensor s n
  -> Tensor t m
  -> Tensor r o
dyad' f t1 t2 =
  let l = rank t1
      s1 = shape t1
      s2 = shape t2
  in generateTensor (\i -> let (ti1,ti2) = splitAt l i in f (getValue t1 s1 ti1) (getValue t2 s2 ti2))

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
  :: ( r ~ (s ++ t)
     , HasShape s
     , HasShape t
     , HasShape r
     , Num n
     , Eq n)
  => Tensor s n -> Tensor t n -> Tensor r n
dyad = dyad' mult


type DotTensor s1 s2 = Init s1 ++ Init s2

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
  :: ( Last s ~ Head s'
     , r ~ DotTensor s s'
     , HasShape s
     , HasShape s'
     , HasShape r
     , Num n
     , Eq n)
  => Tensor s n
  -> Tensor s' n
  -> Tensor r n
dot t1 t2 =
  let s1 = shape t1
      s2 = shape t2
      n  = last s1
      b  = length s1 - 1
      f (!x,!y) = (getValue t1 s1 x) `mult` (getValue t2 s2 y)
  in generateTensor (\i ->
        let (ti1,ti2) = splitAt b i
        in sum $ f <$> [(ti1++[x],x:ti2)| x <- [0..n-1]])

type Contraction s x y = DropIndex (DropIndex s y) x
type DropIndex s i = Take i s ++ Drop (i+1) s

-- | Contraction Tensor
--
-- > λ> a = [1..16] :: Tensor '[4,4] Int
-- > λ> a
-- > [[1,2,3,4],
-- > [5,6,7,8],
-- > [9,10,11,12],
-- > [13,14,15,16]]
-- > λ> contraction (i0,i1) a
-- > 34
--
-- In rank 2 tensor, contraction of tensor is just the <https://en.wikipedia.org/wiki/Trace_(linear_algebra) trace>.
contraction
  :: forall x y s s' n.
     ( CheckIndices x y s
     , s' ~ Contraction s x y
     , Dimension s x ~ Dimension s y
     , KnownNat x
     , KnownNat y
     , HasShape s
     , HasShape s'
     , KnownNat (Dimension s x)
     , Num n)
  => (Proxy x, Proxy y)
  -> Tensor s  n
  -> Tensor s' n
contraction (px, py) t@(Tensor f) =
  let x  = toNat px
      y  = toNat py
      n  = toNat (Proxy :: Proxy (Dimension s x))
      s  = shape t
  in generateTensor (go x (y-x-1) n (f s) )
  where
    {-# INLINE go #-}
    go a b n fs i =
      let (r1,rt) = splitAt a i
          (r3,r4) = splitAt b rt
      in sum $ fmap fs [r1 ++ (j:r3) ++ (j:r4) | j <- [0..n-1]]

type CheckSelect dim i s = (CheckDimension dim s && IsIndex i (Dimension s dim)) ~ 'True

-- | Select `i` indexing of tensor
--
-- > λ> a = identity :: Tensor '[4,4] Int
-- > λ> select (i0,i0) a
-- > [1,0,0,0]
-- > λ> select (i0,i1) a
-- > [0,1,0,0]
select
  :: ( CheckSelect dim i s
     , HasShape s
     , KnownNat dim
     , KnownNat i)
  => (Proxy dim, Proxy i)
  -> Tensor s n
  -> Tensor (DropIndex s dim) n
select (pd, pid) t=
  let dim = toNat pd
      ind = toNat pid
  in transformTensor (go dim ind) t
  where
    {-# INLINE go #-}
    go d i _ (_,i') = let (a,b) = splitAt d i' in a ++ (i:b)

type CheckSlice dim from to s = (CheckDimension dim s && IsIndices from to (Dimension s dim)) ~ 'True
type Slice dim from to s = Take dim s ++ ( to - from : Tail (Drop dim s))

-- | Slice tensor
--
-- > λ> a = identity :: Tensor '[4,4] Int
-- > λ> a
-- > [[1,0,0,0],
-- > [0,1,0,0],
-- > [0,0,1,0],
-- > [0,0,0,1]]
-- > λ> slice (i0,(i1,i3)) a
-- > [[0,1,0,0],
-- > [0,0,1,0]]
-- > λ> slice (i1,(i1,i3)) a
-- > [[0,0],
-- > [1,0],
-- > [0,1],
-- > [0,0]]
slice
  :: ( CheckSlice dim from to s
     , s' ~ Slice dim from to s
     , KnownNat dim
     , KnownNat from
     , KnownNat (to - from)
     , HasShape s)
  => (Proxy dim, (Proxy from, Proxy to))
  -> Tensor s n
  -> Tensor s' n
slice (pd, (pa,_)) t =
  let d = toNat pd
      a = toNat pa
  in transformTensor (\_ (_,i') -> let (x,y:ys) = splitAt d i' in x ++ (y+a:ys)) t

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
     , HasShape s)
  => Tensor s n
  -> Tensor s' n
expand = transformTensor go
  where
    {-# INLINE go #-}
    go s (_, i') = zipWith mod i' s

type CheckConcatenate i a b = (IsIndex i (TensorRank a)) ~ 'True
type Concatenate i a b = Take i a ++ (Dimension a i + Dimension b i : Drop (i+1) a)

-- | Join a sequence of arrays along an existing axis.
--
-- > λ> a = [1..4] :: Tensor '[2,2] Int
-- > λ> a
-- > [[1,2],
-- > [3,4]]
-- > λ> b = [1,1,1,1] :: Tensor '[2,2] Int
-- > λ> b
-- > [[1,1],
-- > [1,1]]
-- > λ> concentrate i0 a b
-- > [[1,2],
-- > [3,4],
-- > [1,1],
-- > [1,1]]
-- > λ> concentrate i1 a b
-- > [[1,2,1,1],
-- > [3,4,1,1]]
concatenate
  :: ( TensorRank a ~ TensorRank b
    , DropIndex a i ~ DropIndex b i
    , CheckConcatenate i a b
    , Concatenate i a b ~ c
    , HasShape a
    , HasShape b
    , KnownNat i)
  => Proxy i
  -> Tensor a n
  -> Tensor b n
  -> Tensor c n
concatenate p ta@(Tensor a) tb@(Tensor b) =
  let i  = toNat p
      sa = shape ta
      sb = shape tb
      n  = sa !! i
  in Tensor $ \_ ind -> let (ai,x:bi) = splitAt i ind in if x >= n then b sb (ai ++ (x-n):bi) else a sa ind

type CheckInsert dim i b = (CheckDimension dim b && IsIndex i (Dimension b dim))  ~ 'True
type Insert dim b = Take dim b ++ (Dimension b dim + 1 : Drop (dim + 1) b)

-- | Insert tensor to higher level tensor
--
-- > λ> a = [1,2] :: Vector 2 Float
-- > λ> b = a `dyad` a
-- > λ> b
-- > [[1.0,2.0],
-- > [2.0,4.0]]
-- > λ> :t b
-- > b :: Tensor '[2, 2] Float
-- > λ> c = [1..4] :: Tensor '[1,2,2] Float
-- > λ> c
-- > [[[1.0,2.0],
-- > [3.0,4.0]]]
-- > λ> d = insert i0 i0 b c
-- > λ> :t d
-- > d :: Tensor '[2, 2, 2] Float
-- > λ> d
-- > [[[1.0,2.0],
-- > [2.0,4.0]],
-- > [[1.0,2.0],
-- > [3.0,4.0]]]
-- > λ> insert i0 i1 b c
-- > [[[1.0,2.0],
-- > [3.0,4.0]],
-- > [[1.0,2.0],
-- > [2.0,4.0]]]
insert
  :: ( DropIndex b dim ~ a
    , CheckInsert dim i b
    , KnownNat i
    , KnownNat dim
    , HasShape a
    , HasShape b)
  => Proxy dim
  -> Proxy i
  -> Tensor a n
  -> Tensor b n
  -> Tensor (Insert dim b) n
insert pd px a@(Tensor ta) b@(Tensor tb) =
  let d = toNat pd
      i = toNat px
      sa = shape a
      sb = shape b
  in Tensor $ \_ ci -> let (xs,n:ys) = splitAt d ci in if n == i then ta sa (xs++ys) else if n < i then tb sb ci else tb sb (xs ++ ((n-1):ys))

-- | Append tensor to the end of some dimension of other tensor
--
-- > λ> a = [1,2] :: Vector 2 Float
-- > λ> a
-- > [1.0,2.0]
-- > λ> b = 3 :: Tensor '[] Float
-- > λ> b
-- > 3.0
-- > λ> append i0 b a
-- > [1.0,2.0,3.0]
append
  :: forall dim a b n.
    ( DropIndex b dim ~ a
    , CheckInsert dim (Dimension b dim) b
    , KnownNat (Dimension b dim)
    , KnownNat dim
    , HasShape a
    , HasShape b)
  => Proxy dim
  -> Tensor a n
  -> Tensor b n
  -> Tensor (Insert dim b) n
append pd = insert pd (Proxy :: Proxy (Dimension b dim))

-- | Convert tensor to untyped function, for internal usage.
runTensor :: HasShape s => Tensor s n -> Index -> n
runTensor t@(Tensor f) = f (shape t)
