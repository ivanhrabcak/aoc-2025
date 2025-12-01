module Day01 where

import GHC.List (scanl')
import Paths_aoc2025 (getDataFileName)

data Dir = L | R deriving (Show)

type Turn = (Dir, Integer)

offset :: Turn -> Integer
offset (L, n) = -n
offset (R, n) = n

parseTurn :: String -> Turn
parseTurn (d : xs) =
  (if d == 'L' then L else R, read xs)
parseTurn _ = error "Invalid input"

positions :: [Turn] -> [Integer]
positions = scanl' step 50
  where
    step pos t = (pos + offset t) `mod` 100

solutionPartOne :: [Turn] -> Int
solutionPartOne = length . filter (== 0) . tail . positions

countIntermediateZeroes :: (Integral a) => a -> a -> Int
countIntermediateZeroes start off = length [() | i <- [1 .. abs off], let pos = (start + signum off * i) `mod` 100, pos == 0]

solutionPartTwo :: [Turn] -> Int
solutionPartTwo turns = go 50 turns 0
  where
    go _ [] total = total
    go pos (x : xs) total =
      let off = offset x
          interZeroes = countIntermediateZeroes pos off
          newPos = (pos + off) `mod` 100
       in go newPos xs (total + interZeroes)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)

  let dialTurns = map parseTurn inputLines
  print $ solutionPartOne dialTurns
  print $ solutionPartTwo dialTurns
