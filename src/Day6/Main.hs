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

--- Part Two ---

Your device's communication system is correctly detecting packets, but still isn't working. It looks like it also needs to look for messages.

A start-of-message marker is just like a start-of-packet marker, except it consists of 14 distinct characters rather than 4.

Here are the first positions of start-of-message markers for all of the above examples:

    mjqjpqmgbljsphdztnvjfqwrcgsmlb: first marker after character 19
    bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 23
    nppdvjthqldpwncqszvftbrmjlhg: first marker after character 23
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 29
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 26

How many characters need to be processed before the first start-of-message marker is detected?
-}
module Day6.Main where

import qualified Test.Hspec as Test
import qualified Data.List.Split as Split
import qualified Data.List as L
import qualified Data.Maybe as Maybe

part1 :: IO ()
part1 = do
  putStr "Part One: "
  input <- readFile "src/Day6/input"
  print $ findMarkerPosition 4 input

part2 :: IO ()
part2 = do
  putStr "Part Two: "
  input <- readFile "src/Day6/input"
  print $ findMarkerPosition 14 input

test :: IO ()
test = Test.hspec $ do
  Test.describe "findMarkerPosition" $ do
    Test.it "finds the position of the first marker" $ do
      findMarkerPosition 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `Test.shouldBe` 5
      findMarkerPosition 4 "nppdvjthqldpwncqszvftbrmjlhg" `Test.shouldBe` 6
      findMarkerPosition 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `Test.shouldBe` 10
      findMarkerPosition 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `Test.shouldBe` 11
      findMarkerPosition 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `Test.shouldBe` 19
      findMarkerPosition 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" `Test.shouldBe` 23
      findMarkerPosition 14 "nppdvjthqldpwncqszvftbrmjlhg" `Test.shouldBe` 23
      findMarkerPosition 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `Test.shouldBe` 29
      findMarkerPosition 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `Test.shouldBe` 26

findMarkerPosition :: Int -> String -> Int
findMarkerPosition n = (+ n) . Maybe.fromJust . L.findIndex distinct . Split.divvy n 1

distinct :: Eq a => [a] -> Bool
distinct xs = length (L.nub xs) == length xs
