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
    findCandidates n xs = tryCandidates n xs candidates
      where
        candidates = sortBy (flip compare) (nub xs)

    tryCandidates _ _ [] = []
    tryCandidates n xs (m : ms) =
      if length maximums == n
        then maximums
        else tryCandidates n xs ms
      where
        rest = drop 1 $ dropWhile (/= m) xs
        maximums = m : findCandidates (n - 1) rest

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
