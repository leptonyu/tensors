-- |
-- Module:      Data.Tensor
-- Copyright:   (c) 2018 Daniel YU
-- License:     BSD3
-- Maintainer:  Daniel YU <leptonyu@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Tensor In Haskell
--
-- In ghci
--
-- > λ> :set -XDataKinds
-- > λ> :set -XOverloadedLists
-- > λ> import Data.Tensor
-- > λ> a = identity :: Tensor '[3,3] Int
-- > λ> a
-- > [[1,0,0],
-- > [0,1,0],
-- > [0,0,1]]
-- > λ> b = [1..9] :: Tensor '[3,3] Int
-- > λ> b
-- > [[1,2,3],
-- > [4,5,6],
-- > [7,8,9]]
-- > λ> a + b
-- > [[2,2,3],
-- > [4,6,6],
-- > [7,8,10]]
-- > λ> a - b
-- > [[0,-2,-3],
-- > [-4,-4,-6],
-- > [-7,-8,-8]]
-- > λ> a * b
-- > [[1,0,0],
-- > [0,5,0],
-- > [0,0,9]]
-- > λ> a `dot` b
-- > [[1,2,3],
-- > [4,5,6],
-- > [7,8,9]]
-- > λ> :t a `dyad` b
-- > a `dyad` b :: Tensor '[3, 3, 3, 3] Int
-- > λ> contraction a (i0,i1)
-- > 3
-- > λ> :t contraction a (i0,i1)
-- > contraction a (i0,i1) :: Tensor '[] Int
-- > λ> select a (i0,i0)
-- > [1,0,0]
-- > λ> select a (i0,i1)
-- > [0,1,0]
-- > λ> select a (i0,i2)
-- > [0,0,1]
-- > λ> c = 1 :: Tensor '[3,3] Int
-- > λ> c
-- > [[1,1,1],
-- > [1,1,1],
-- > [1,1,1]]
-- > λ> d = [1..4] :: Tensor '[2,2] Int
-- > λ> d
-- > [[1,2],
-- > [3,4]]
-- > λ> transpose d
-- > [[1,3],
-- > [2,4]]

module Data.Tensor(
  -- * Tensor Definition
    Tensor
  , identity
  , Scalar
  , Vector
  , Matrix
  , SimpleTensor
  -- ** Tensor Index
  , TensorIndex
  , Index
  -- * Tensor Dimension
  , TensorRank
  , Shape
  , shape
  , rank
  , HasShape(..)
  -- * Tensor Operation
  -- ** Reshape Tensor
  , reshape
  -- ** Clone Tensor
  , clone
  -- ** Transpose Tensor
  , Transpose
  , transpose
  , Swapaxes
  , swapaxes
  -- ** Dyadic Tensor
  , dyad'
  , dyad
  -- ** Tensor Product
  , DotTensor
  , dot
  -- ** Contraction Tensor
  , Contraction
  , Dimension
  , DropIndex
  , contraction
  -- ** Tensor Selection
  , (!)
  , CheckDimension
  , CheckSelect
  , select
  , CheckSlice
  , Slice
  , slice
  -- ** Tensor Manipulation
  , expand
  , CheckConcatenate
  , Concatenate
  , concatenate
  , CheckInsert
  , Insert
  , insert
  , append
  -- ** Tensor Space
  , linspace
  , geospace
  , logspace
  , grid
  , meshgrid2
  , meshgrid3
  , meshgrid4
  , meshgrid5
  , meshgrid6
  , meshgrid7
  , meshgrid8
  , meshgrid9
  -- * Matrix Operation
  , SimpleMatrix
  , det
  , lu
  , det'
  , dotM
  , trace
  , diag
  -- * Statistical Functions
  , average
  , var
  , std
  -- * Helper
  , runTensor
  , i0
  , i1
  , i2
  , i3
  , i4
  , i5
  , i6
  , i7
  , i8
  , i9
  ) where

import           Data.Tensor.Matrix
import           Data.Tensor.Space
import           Data.Tensor.Statistics
import           Data.Tensor.Tensor
import           Data.Tensor.Type
