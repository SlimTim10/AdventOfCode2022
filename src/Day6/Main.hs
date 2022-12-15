{-
To fix the communication system, you need to add a subroutine to the device that detects a start-of-packet marker in the datastream. In the protocol being used by the Elves, the start of a packet is indicated by a sequence of four characters that are all different.

The device will send your subroutine a datastream buffer (your puzzle input); your subroutine needs to identify the first position where the four most recently received characters were all different. Specifically, it needs to report the number of characters from the beginning of the buffer to the end of the first such four-character marker.

For example, suppose you receive the following datastream buffer:

mjqjpqmgbljsphdztnvjfqwrcgsmlb

After the first three characters (mjq) have been received, there haven't been enough characters received yet to find the marker. The first time a marker could occur is after the fourth character is received, making the most recent four characters mjqj. Because j is repeated, this isn't a marker.

The first time a marker appears is after the seventh character arrives. Once it does, the last four characters received are jpqm, which are all different. In this case, your subroutine should report the value 7, because the first start-of-packet marker is complete after 7 characters have been processed.

Here are a few more examples:

    bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
    nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11

How many characters need to be processed before the first start-of-packet marker is detected?
-}
module Day6.Main where

import qualified Test.Hspec as Test
import qualified Data.List.Split as Split

part1 :: IO ()
part1 = do
  putStr "Part One: "
  input <- readFile "src/Day6/input"
  print $ findMarkerPosition input

test :: IO ()
test = Test.hspec $ do
  Test.describe "findMarkerPosition" $ do
    Test.it "finds the position of the first marker" $ do
      findMarkerPosition "bvwbjplbgvbhsrlpgdmjqwftvncz" `Test.shouldBe` 5
      findMarkerPosition "nppdvjthqldpwncqszvftbrmjlhg" `Test.shouldBe` 6
      findMarkerPosition "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `Test.shouldBe` 10
      findMarkerPosition "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `Test.shouldBe` 11

findMarkerPosition :: String -> Integer
findMarkerPosition = fst . head . dropWhile (not . distinct . snd) . zip [4..] . Split.divvy 4 1

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
