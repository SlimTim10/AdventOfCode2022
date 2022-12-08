module Main where

import qualified Day1.Main as Day1
import qualified Day2.Main as Day2
import qualified Day3.Main as Day3

main :: IO ()
main = do
  putStrLn "Day1"
  Day1.test
  Day1.part1
  Day1.part2
  
  putStrLn "Day2"
  Day2.test
  Day2.part1
  Day2.part2

  putStrLn "Day3"
  Day3.test
  Day3.part1
  Day3.part2
