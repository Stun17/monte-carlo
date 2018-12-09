module Shuffle (shuffle) where

import System.Random
import Data.List (nub)

shuffle :: IO [Int]
shuffle = genIntSeed >>= \seed -> return $ makeLstInt seed 

genIntSeed :: IO Int
genIntSeed = 
  getStdGen >>= \g0 ->
  let (x, g) = randomR (0, 1048576) g0
  in setStdGen g >> return x 

makeLstInt :: Int -> [Int]
makeLstInt seed = 
  take 25 $ nub $ take 64 $ randomRs (0, 51) $ mkStdGen seed 
