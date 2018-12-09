  -- аукцион "русская рулетка"

  -- two players bid value1 and value2
  -- six steps with probabilities of termination 1/6 1/5 1/4 1/3 1/2 1
  -- before his round starts player can pass and win=0
  -- if not - revolver is tested for shooting for him
  --             if true  - player lost=-1
  --             if false - next player has his round

module Main where

import System.Random
import Control.Monad
import Control.Monad.Cont 
import System.Environment

type Result a = (Integer, (a, a))

-- arguments : number of emulations, bid1 bid2
-- output    : String in form: "step: n who_win"
main :: IO [()]
main =
  -- 0 < alpha , beta 
  getArgs >>= \ (num : alpha : beta : rest) -> 
  let bidOne = read alpha
      bidTwo = read beta
      n = read num
  in  replicateM n $
      guard (0 < bidOne && 0 < bidTwo) >>
      getBullet >>= \ revolver -> 
      (return $ auction revolver bidOne bidTwo) >>= \(step, (winOne, winTwo)) ->
         putStrLn $ "step " ++ (show step) ++ 
                 (if winOne < 0 then ": 1st killed" else
                    if winOne == 0 then ": 1st passed" else
                      if winTwo < 0 then ": 2nd killed" else
                        if winTwo == 0 then ": 2nd passed" else "this never will be shown")

-- random number - place of catriage in revolver      
getBullet :: IO Integer
getBullet =
  getStdGen >>= \g1 ->
  let (x, g2) = randomR (1, 6::Integer) g1
  in setStdGen g2 >> return x

-- проведение аукциона - раунд за раундом до первого отказа или первого выстрела
--     input: bullet_place  bid_01  bid_02
--     output: (step, reward01, reward02)
auction :: Integer -> Double -> Double -> Result Double
auction baraban bid01 bid02 = (`runCont` id) $ callCC $ \ foo ->
  bar 0.17 0.20 1 bid01 bid02 foo >>
  bar 0.20 0.25 2 bid02 bid01 foo >> 
  bar 0.25 0.33 3 bid01 bid02 foo >> 
  bar 0.33 0.50 4 bid02 bid01 foo >> 
  bar 0.50 1.00 5 bid01 bid02 foo >>
  bar 1.00 2.00 6 bid02 bid01 foo >>
  return (0, (0, 0)) -- this point cannot be achieved
  where
    -- round in progress
    --    params : probability_one_shoot probability_another_shoot round
    --             bid_one bid_another  dummy_function_for_continuation 
    bar :: Monad m => Double -> Double -> Integer -> Double -> Double -> 
                      (Result Double -> m ()) -> m ()  
    bar p1 p2 step x y f =
      let k1 = if (odd step) then ( 0, y) else (y,  0)
          k2 = if (odd step) then (-1, y) else (y, -1)          
      in  unless (decision p1 $ estimation p2 x) (f (step, k1)) >>
          when (baraban == step) (f (step, k2)) 

-- решение продолжать борьбу    
--   input  : own_shoot_brobability    my_prize
--   output : yes/no
decision ::  Double -> Double -> Bool
decision p w = p * w > 1 - p 

-- матожидание выйгрыша при продолжении борьбы
--   input  : opponent_shoot_probability      my_prize
--   output : value
estimation :: Double -> Double -> Double
estimation p b
  | p == 0.20  = currentEstim p * b + (1 - p) * (1 - p) * estimation 0.33 b
  | p == 0.25  = currentEstim p * b + (1 - p) * (1 - p) * estimation 0.50 b             
  | p == 0.33  = currentEstim p * b + (1 - p) * (1 - p) * estimation 1.00 b
  | p == 0.50  = currentEstim p * b 
  | otherwise = b
  where
    currentEstim k = (k + (1 - k) * k) 
