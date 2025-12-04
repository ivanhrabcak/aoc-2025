module Day03 where

import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List (nub, sortBy)
import Paths_aoc2025 (getDataFileName)

digit :: Char -> Integer
digit x = toInteger (ord x - ord '0')

concatN :: [Integer] -> Integer
concatN = foldl' (\acc x -> acc * 10 + x) 0

parseJoltages :: [String] -> [[Integer]]
parseJoltages = map (digit <$>)

orderedMaximums :: (Ord a) => Int -> [a] -> [a]
orderedMaximums = findCandidates
  where
    findCandidates 0 _ = []
    findCandidates _ [] = []
    findCandidates k ys = tryCandidates k ys candidates
      where
        candidates = sortBy (flip compare) (nub ys)

    tryCandidates _ _ [] = []
    tryCandidates k ys (m : ms) =
      if length maximums == k
        then maximums
        else tryCandidates k ys ms
      where
        rest = drop 1 $ dropWhile (/= m) ys
        maximums = m : findCandidates (k - 1) rest

sumOrderedMaximums :: Int -> [[Integer]] -> Integer
sumOrderedMaximums n joltages = sum $ map (concatN . orderedMaximums n) joltages

partOneSolution :: [[Integer]] -> Integer
partOneSolution = sumOrderedMaximums 2

partTwoSolution :: [[Integer]] -> Integer
partTwoSolution = sumOrderedMaximums 12

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)

  let joltages = parseJoltages inputLines
  print $ partOneSolution joltages
  print $ partTwoSolution joltages
