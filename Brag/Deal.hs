module Deal where

import Shuffle 

-- input  : num - number of players in Brag
-- output : list of hands, each hand is list of cards
deal :: Int -> IO [[Int]]
deal num = shuffle >>= \deck -> return $ hands num deck

-- input  : whom - place of the gamer on the table fm 1 to 8
--          num  - number of players
--          deck - shuffled deck of cards
-- output : list of cards for this player
hand :: Int -> Int -> [Int] -> [Int]
hand whom num deck = 
  (take 1 $ drop (whom - 1) deck) >>= \ card1 -> 
  (take 1 $ drop (num + whom - 1) deck) >>= \ card2 -> 
  (take 1 $ drop (2 * num + whom - 1) deck) >>= \ card3 -> 
  [card1, card2, card3] 

-- input  : num - number of players
--          deck - shuffled deck of cards
-- output : list of hands, each hand is list of cards         
hands :: Int -> [Int] -> [[Int]]
hands num deck = 
  map (\x -> hand x num deck)  [1 .. num] 
