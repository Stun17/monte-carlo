module Main where

import Control.Monad.State
import System.Environment


-- input  :  price for additional path
--           num of vehicles
--           initial state vehicles01
--           initial state vehicles02
-- output :  (vehicles01, vehicles02)
main :: IO ()
main =
  getArgs >>= \ (prc:lim:ini1:ini2:_) ->
  let init01 = read ini1
      init02 = read ini2
      limit  = read lim
      price  = read prc
      count  = init01 + init02
  in  guard (limit > count) >> -- prg must terminate
      (print $ doitnow limit price (count, (init01, init02)))

type Traffic = (Integer, Integer)

doitnow :: Integer -> Double -> (Integer, Traffic) -> Traffic
doitnow k price33 (n, g@(st1, st2))
  | n >= k = g
  | otherwise = doitnow k price33 $ runState (action price33) g

-- price for path
price11, price12, price21, price22 :: Traffic -> Double
price11 _ = 10
price12 (x, y) = (fromInteger x) / (fromInteger $ x + y)
price21 (x, y) = (fromInteger y) / (fromInteger $ x + y)
price22 _ = 10

-- which path is cheaper
decision :: Double -> Traffic -> Bool
decision price33 x =
  let path11  = price11 x + price12 x
      path12  = price11 x + price33 + price22 x
      path21  = price21 x + price22 x
      path22  = price21 x + price33 + price12 x
      ps      = [path11, path12, path21, path22]
      minpath = minimum ps
      alloc   = map (\x -> if x > minpath then 0 else 1) ps
  in case alloc of
    [_,_,0,0] -> True
    _         -> False

action :: Double -> State Traffic Integer
action price33 =  
  get >>= \ st@(st1, st2) ->
  (if decision price33 st then put (st1 + 1, st2) else put (st1, st2 + 1)) >>
  return (st1 + st2 + 1)
