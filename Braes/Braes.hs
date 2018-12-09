module Main where

import Control.Monad.State

-- input  :  price for additional path
--           num of vehicles
--           initial state (vehicles01, vehicles02)
-- output :  (vehicles01, vehicles02)
main :: IO ()
main =
  readLn >>= \ route33 ->
  readLn >>= \ limit ->
  readLn >>= \ (init01, init02) ->
  let count = init01 + init02 in
  guard (limit > count) >> -- prg must terminate
  (print $ doitnow limit route33 (count, (init01, init02))) >> main

type Traffic = (Integer, Integer)

doitnow :: Integer -> Double -> (Integer, Traffic) -> Traffic
doitnow k route33 (n, g@(st1, st2))
  | n >= k = g
  | otherwise = doitnow k route33 $ runState (action route33) g

-- price for path
route11, route12, route21, route22 :: Traffic -> Double
route11 _ = 1
route12 (x, y) = (fromInteger x) / (fromInteger $ x + y)
route21 (x, y) = (fromInteger y) / (fromInteger $ x + y)
route22 _ = 1

-- which path is cheaper
decision :: Double -> Traffic -> Bool
decision route33 x =
  let path11  = route11 x + route12 x
      path12  = route11 x + route33 + route22 x
      path21  = route21 x + route22 x
      path22  = route21 x + route33 + route12 x
      ps      = [path11, path12, path21, path22]
      minpath = minimum ps
      alloc   = map (\x -> if x > minpath then 0 else 1) ps
  in case alloc of
    [_,_,0,0] -> True
    _         -> False

action :: Double -> State Traffic Integer
action route33 =  
  get >>= \ st@(st1, st2) ->
  (if decision route33 st then put (st1 + 1, st2) else put (st1, st2 + 1)) >>
  return (st1 + st2 + 1)
