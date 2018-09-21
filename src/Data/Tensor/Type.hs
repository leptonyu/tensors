{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tensor.Type where

import           Data.List                    (foldl')
import           Data.Proxy
import           Data.Reflection
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           GHC.TypeLits
import           Unsafe.Coerce

newtype MagicNats r = MagicNats (forall (n :: [Nat]). SingI n => Proxy n -> r)
reifyNats :: forall r. [Int] -> (forall (n :: [Nat]). SingI n => Proxy n -> r) -> r
reifyNats n k = unsafeCoerce (MagicNats k :: MagicNats r) n Proxy

instance SingI a => Reifies (a::[Nat]) [Int] where
  reflect = natsVal

type Shape = [Int]
type Index = [Int]

toNat :: KnownNat s => Proxy s -> Int
toNat = unsafeCoerce . natVal

natsVal :: forall proxy (s::[Nat]). SingI s => proxy s -> Index
natsVal _ = case (sing :: Sing s) of
  SNil         -> []
  (SCons x xs) -> unsafeCoerce <$> (fromSing x: fromSing xs)

viToti :: Index -> Int -> Index
viToti s i = snd $ foldl' go (i,[]) (reverse s)
  where
    {-# INLINE go #-}
    go (i',x) n = let (d,r) = divMod i' n in (d,r:x)

tiTovi :: Index -> Index -> Int
tiTovi = go 0
  where
    {-# INLINE go #-}
    go i (n:ns) (ind:inds) = go (i * n + ind) ns inds
    go i _ _               = i

mult :: (Eq a, Num a) => a -> a -> a
mult a b = case a of
  0 -> 0
  c -> c * b

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
