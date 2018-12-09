module Main where

import Shuffle
import Deal

main = shuffle >>= print >> deal 4 >>= print
