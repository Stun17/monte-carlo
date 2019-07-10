module Main where

import Control.Monad.State
import System.Environment

-- input  :  price for additional middle path
--           max num of vehicles on all paths 
--           initial state - num of vehicles to point 1
--           initial state - num of vehicles to point 2
-- output :  resulting division (vehicles to 1, vehicles to 2)
main :: IO ()
main =
  getArgs >>= \ (prc:lim:iniX:iniY:_) ->
  let initX = read iniX
      initY = read iniY
      limit = read lim
      price = read prc
      quantity = initX + initY
  in  guard (limit > quantity) >> -- because the program must terminate :)
      (print $ calculate limit price (quantity, (initX, initY)))

type Traffic = (Integer, Integer)

calculate :: Integer -> Double -> (Integer, Traffic) -> Traffic
calculate limit price3 (quantity, state@(state1, state2))
  | quantity >= limit = state 
  | otherwise = calculate limit price3 $ runState (action price3) state 

-- prices for paths
price_a1, price_1b, price_a2, price_2b :: Traffic -> Double
price_a1 _ = 10
price_1b (x, y) = 20 * (fromInteger x) / (fromInteger $ x + y)
price_a2 (x, y) = 20 * (fromInteger y) / (fromInteger $ x + y)
price_2b _ = 10

-- which path is cheaper?
decision :: Double -> Traffic -> Bool
decision price3 s =
  let path_a1b  = price_a1 s + price_1b s
      path_a12b = price_a1 s + price3 + price_2b s
      path_a2b  = price_a2 s + price_2b s
      path_a21b = price_a2 s + price3 + price_1b s
      ps        = [path_a1b, path_a12b, path_a2b, path_a21b]
      minpath   = minimum ps
      alloc     = map (\z -> if z > minpath then 0 else 1) ps
  in case alloc of
    [1,0,0,0] -> True
    [0,1,0,0] -> True 
    [0,0,1,0] -> False 
    [0,0,0,1] -> False 

    [1,1,0,0] -> True
    [1,0,1,0] -> True 
    [0,1,1,0] -> True 
    [1,0,0,1] -> False 
    [0,1,0,1] -> False 
    [0,0,1,1] -> False 

    [1,1,1,0] -> True
    [1,1,0,1] -> True
    [1,0,1,1] -> False 
    [0,1,1,1] -> False 

    [1,1,1,1] -> True 
    _         -> False

action :: Double -> State Traffic Integer
action price3 =  
  get >>= \ state@(state1, state2) ->
  ( if decision price3 state 
    then put (state1 + 1, state2) 
    else put (state1, state2 + 1) ) >>
  return (state1 + state2 + 1)
