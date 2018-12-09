module Main where

import Shuffle 
import Control.Monad (replicateM)

test_shuffle = 
  (replicateM 100000 $ 
     shuffle >>= \xs -> 
     (return $ any (<52) xs) >>= \rs01 ->
     (return $ 25 == length xs) >>= \rs02 ->
     (return $ (True == rs02) && (True == rs01))
  ) >>= \rs99 -> putStrLn $ if any (==True) rs99 then "ok" else "failed" 

main = test_shuffle
