-- {-# LANGUAGE QuasiQuotes #-}

module Day02 where

import Paths_aoc2025 (getDataFileName)
import Text.Regex.PCRE ((=~))

type IDRange = (Integer, Integer)

parseRanges :: String -> [IDRange]
parseRanges "" = []
parseRanges s = (read startId, read endId) : parseRanges (drop 1 xs)
  where
    (startId, rest) = span (/= '-') s
    (endId, xs) = span (/= ',') (tail rest)

findAllMatches :: String -> [Integer] -> [Integer]
findAllMatches regexp = filter (\i -> show i =~ regexp)

countMatchedIds :: String -> [IDRange] -> Integer
countMatchedIds regexp rs = sum $ concatMap (\(start, end) -> findAllMatches regexp [start .. end]) rs

partOneSolution :: [IDRange] -> Integer
partOneSolution = countMatchedIds "^(\\d+)\\1$"

partTwoSolution :: [IDRange] -> Integer
partTwoSolution = countMatchedIds "^(\\d+)\\1+$"

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)

  let ranges = parseRanges (head inputLines)

  print $ partOneSolution ranges
  print $ partTwoSolution ranges
