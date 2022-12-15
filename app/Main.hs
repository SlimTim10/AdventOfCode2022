module Main where

import qualified Day1.Main as Day1
import qualified Day2.Main as Day2
import qualified Day3.Main as Day3
import qualified Day4.Main as Day4
import qualified Day5.Main as Day5
import qualified Day6.Main as Day6

main :: IO ()
main = do
  putStrLn "Day1"
  Day1.test
  Day1.part1
  Day1.part2
  putStrLn "---"
  
  putStrLn "Day2"
  Day2.test
  Day2.part1
  Day2.part2
  putStrLn "---"

  putStrLn "Day3"
  Day3.test
  Day3.part1
  Day3.part2
  putStrLn "---"

  putStrLn "Day4"
  Day4.test
  Day4.part1
  Day4.part2
  putStrLn "---"

  putStrLn "Day5"
  Day5.test
  Day5.part1
  Day5.part2
  putStrLn "---"

  putStrLn "Day6"
  Day6.test
  Day6.part1
  Day6.part2
  putStrLn "---"
