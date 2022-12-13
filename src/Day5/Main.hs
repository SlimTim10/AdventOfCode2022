{-
They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.

Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:

[D]        
[N] [C]    
[Z] [M] [P]
 1   2   3 

In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:

        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3

Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:

        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3

Finally, one crate is moved from stack 1 to stack 2:

        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3

The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.

After the rearrangement procedure completes, what crate ends up on top of each stack?
-}
module Day5.Main where

import qualified Test.Hspec as Test
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Control.Monad as M
import qualified Data.List as L

part1 :: IO ()
part1 = do
  putStr "Part One: "
  input <- readFile "src/Day5/input"
  let stacks = parseStacks . takeWhile (not . (" 1" `L.isPrefixOf`)) $ lines input
  let moves = dropWhile (not . ("move" `L.isPrefixOf`)) $ lines input
  print
    $ map (last . snd)
    . Map.toList
    . L.foldl' f stacks
    $ moves
  where
    f :: Stacks -> String -> Stacks
    f stacks ln = moveCrates n fromKey toKey stacks
      where (n, fromKey, toKey) = parseMoveLine ln

test :: IO ()
test = Test.hspec $ do
  Test.describe "pop" $ do
    Test.it "takes the top crate off a stack" $ do
      pop ['M', 'C', 'D'] `Test.shouldBe` ('D', ['M', 'C'])
      pop ['M'] `Test.shouldBe` ('M', [])
  Test.describe "push" $ do
    Test.it "puts a crate on top of a stack" $ do
      push 'D' ['M', 'C'] `Test.shouldBe` ['M', 'C', 'D']
      push 'M' [] `Test.shouldBe` ['M']
  Test.describe "moveCrate" $ do
    Test.it "moves a crate from one stack to another" $ do
      let stacks = Map.fromList
            [ (1, ['Z', 'N'])
            , (2, ['M', 'C', 'D'])
            , (3, ['P'])
            ]
      let newStacks = Map.fromList
            [ (1, ['Z', 'N', 'D'])
            , (2, ['M', 'C'])
            , (3, ['P'])
            ]
      moveCrate 2 1 stacks `Test.shouldBe` newStacks
  Test.describe "moveCrates" $ do
    Test.it "moves crates from one stack to another" $ do
      let stacks = Map.fromList
            [ (1, ['Z', 'N', 'D'])
            , (2, ['M', 'C'])
            , (3, ['P'])
            ]
      let newStacks = Map.fromList
            [ (1, [])
            , (2, ['M', 'C'])
            , (3, ['P', 'D', 'N', 'Z'])
            ]
      moveCrates 3 1 3 stacks `Test.shouldBe` newStacks
  Test.describe "parseStacks" $ do
    Test.it "parses lines of strings into stacks" $ do
      let lines =
            [ "    [C]         [Q]         [V]    "
            , "    [D]         [D] [S]     [M] [Z]"
            , "    [G]     [P] [W] [M]     [C] [G]"
            , "    [F]     [Z] [C] [D] [P] [S] [W]"
            , "[P] [L]     [C] [V] [W] [W] [H] [L]"
            , "[G] [B] [V] [R] [L] [N] [G] [P] [F]"
            , "[R] [T] [S] [S] [S] [T] [D] [L] [P]"
            , "[N] [J] [M] [L] [P] [C] [H] [Z] [R]"
            ]
      let stacks = Map.fromList
            [ (1, "NRGP")
            , (2, "JTBLFGDC")
            , (3, "MSV")
            , (4, "LSRCZP")
            , (5, "PSLVCWDQ")
            , (6, "CTNWDMS")
            , (7, "HDGWP")
            , (8, "ZLPHSCMV")
            , (9, "RPFLWGZ")
            ]
      parseStacks lines `Test.shouldBe` stacks

type Crate = Char
type Stack = [Crate] -- bottom to top

pop :: Stack -> (Crate, Stack)
pop s = (last s, init s)

push :: Crate -> Stack -> Stack
push c s = s ++ [c]

type Stacks = Map.Map Int Stack

moveCrate :: Int -> Int -> Stacks -> Stacks
moveCrate fromKey toKey stacks = Map.update (Just . const toStack') toKey . Map.update (Just . const fromStack') fromKey $ stacks
  where
    fromStack = stacks ! fromKey
    (c, fromStack') = pop fromStack
    toStack = stacks ! toKey
    toStack' = push c toStack

moveCrates :: Int -> Int -> Int -> Stacks -> Stacks
moveCrates n fromKey toKey stacks =
  fst
  . until ((== 0) . snd) step
  $ (stacks, n)
  where
    step :: (Stacks, Int) -> (Stacks, Int)
    step (stacks', n') = (moveCrate fromKey toKey stacks', n' - 1)

parseStacks :: [String] -> Stacks
parseStacks = L.foldl' step Map.empty . reverse
  where
    step :: Stacks -> String -> Stacks
    step stacks s = L.foldl' addCrate stacks (parseCrateLine s)

    addCrate :: Stacks -> (Int, Crate) -> Stacks
    addCrate stacks (k, c) = Map.alter f k stacks
      where
        f :: Maybe Stack -> Maybe Stack
        f Nothing = Just (push c [])
        f (Just stack) = Just (push c stack)

parseCrateLine :: String -> [(Int, Crate)]
parseCrateLine ln =
  filter (Char.isLetter . snd)
  . map (\k -> (k, ln !! toIndex k))
  $ [1 .. 9]
  where
    toIndex :: Int -> Int
    toIndex = (+1) . (*4) . (subtract 1)

parseMoveLine :: String -> (Int, Int, Int)
parseMoveLine ln = (n, fromKey, toKey)
  where
    parts = words ln
    n = read $ parts !! 1
    fromKey = read $ parts !! 3
    toKey = read $ parts !! 5
