module Main

import System
import Effects
import Effect.Random
import Effect.Exception
import Effect.StdIO

data MErr = Not_A_Number | Out_Of_Range

parseNumber : Int -> String -> Eff Int [EXCEPTION MErr]
parseNumber num str = 
  if all isDigit (unpack str) 
  then let x = cast str 
       in if (x >= 0 && x <= num) 
          then pure x
          else raise Out_Of_Range
  else raise Not_A_Number

guess : Int -> Eff () [STDIO]
guess t =
  case run (parseNumber 100 (trim !getStr)) of
    Nothing => 
      putStrLn "на входе - белиберда" >>= \_ => guess t
    Just v => 
      case compare v t of
        LT => putStrLn ((cast v) ++ " с нехваткой")             >>= \_ => guess t
        GT => putStrLn ((cast v) ++ " с лишком   ")             >>= \_ => guess t
        EQ => putStrLn $ " ура! это действительно " ++ (cast v)

game : Integer -> Eff () [RND, STDIO]
game n = srand n >>= \_ => guess (fromInteger !(rndInt 0 100))

main : IO ()
main = putStrLn "угадайте число от 1 до 100" >>= \_ => System.time >>= \n => run (game n) 

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:

-- idris -p effects --codegen javascript myrand.idr -o game.js 
-- idris -p effects --codegen c myrand.idr -o game.out 
