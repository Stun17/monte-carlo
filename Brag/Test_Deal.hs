module Main where

import Deal
import Data.List (nub)
import Control.Monad (replicateM , join)

main :: IO () 
main = (replicateM 10000 test_hands_01) >>= print . (all (==True)) 

test_hands_01 :: IO Bool
test_hands_01 = 
  deal 5 >>= \xs -> 
  return $ 
    let k = length $ nub $ join xs  
    in k == 15
