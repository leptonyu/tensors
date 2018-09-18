{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Tensor.Type
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec


spec :: Spec
spec = do
  describe "Data.Tensor" specTensor


specTensor = do
  context "viToti" $ do
    it "quickCheck" $ property $
      \s0 -> let s = take 5 $ fmap (`mod` 10) s0
                 n = [0..product s - 1]
            in fmap (tiTovi s . viToti s) n == n
