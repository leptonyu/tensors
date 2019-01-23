{-# LANGUAGE OverloadedLists #-}
module Main where

import           Criterion.Main
import           Data.Tensor

base = [1..10000] :: Tensor '[100,100] Int
b2se = clone base
b3se = [1..100] :: Tensor '[10,10] Int

main = defaultMain
  [ bgroup "tensor"
    [ bench "identity"    $ nf id (identity :: Tensor '[100,100] Int)
    , bench "clone"       $ nf clone base
    , bench "dyad"        $ nf id (b3se  `dyad` b3se)
    , bench "dot"         $ nf id (base  `dot`  base)
    , bench "dot-2"       $ nf id (b2se  `dot`  b2se)
    , bench "contraction" $ nf (contraction (i0,i1)) base
    ]
  ]
