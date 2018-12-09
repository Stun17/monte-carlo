module Main where

import System.Random
import Control.Monad
import qualified Data.List as L

main :: IO [()]
main =
  (return allVariants) >>= \bs ->
  mapM (oneCalc 10000) bs >>= \xs ->
  mapM print $ L.reverse $ L.sort $ zip (map sum xs) bs

allVariants :: [(Int,Int,Int)]
allVariants =
  [0 .. 3] >>= \x ->
  [0 .. 3 - x] >>= \y ->
  [0 .. 3 - (x + y)] >>= \z ->
  guard (x + y + z == 3) >> return (x, y, z)
           
choice :: Int -> (Int, Int, Int)
choice n = allVariants !! n

genPairInt :: IO (Int, Int)
genPairInt = getStdGen >>= \g1 ->
  let (x, g2) = randomR (0, 9::Int) g1
      (y, g3) = randomR (0, 9::Int) g2
  in setStdGen g2 >> return (x, y) 

oneCalc :: Int -> (Int, Int, Int) -> IO [Int]
oneCalc k xs = replicateM k $ genPairInt >>= \(_,m) -> return $ whoWin xs (choice m)

whoWin :: (Int,Int,Int) -> (Int,Int,Int) -> Int
whoWin (x1,x2,x3) (y1,y2,y3) =
  let summ = (compare x1 y1, compare x2 y2, compare x3 y3)
  in case summ of
    (GT,GT,_ ) -> 1
    (GT,_ ,GT) -> 1
    (_ ,GT,GT) -> 1    
    (EQ,EQ,EQ) -> 0
    (GT,EQ,LT) -> 0
    (LT,EQ,GT) -> 0
    (EQ,GT,LT) -> 0
    (EQ,LT,GT) -> 0
    (GT,LT,EQ) -> 0
    (LT,GT,EQ) -> 0
    _          -> -1
    
    
