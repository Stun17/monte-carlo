module Vickrey where

main :: IO ()
main = readLn >>= \ xs -> (print $ payment xs) >> (print $ utility xs)

allocation :: [Double] -> [Integer]
allocation values =
  let largest = maximum values
  in  map (\ x -> if x < largest then 0 else 1) values

payment :: [Double] -> [Double]
payment values =
  let largest = maximum values
      nextval = maximum $ filter (\ x -> x /= largest) values
  in map (\ x -> (fromInteger x) * nextval) (allocation values)

utility :: [Double] -> [Double]
utility vs = zipWith3 (\v x p -> v * (fromInteger x) - p) vs (allocation vs) (payment vs)
