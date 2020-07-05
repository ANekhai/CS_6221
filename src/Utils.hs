module Utils (
    groupsOf,
    takeNth,
    toString,
    readLabel
)
where

import Data.Vector (Vector, generate)
import Data.Map (Map, elems, singleton, insert, (!))
import System.Random


groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

takeNth :: Int -> [a] -> [a]
takeNth n = go 0
  where
    go _ [] = []
    go _ [x] = [x]
    go 0 (x : xs) = x : go n xs
    go i (x : xs) = go (i - 1) xs

toString :: (Show a) => [a] -> String
toString = unwords . fmap (show)

labelHelper :: Int -> Int -> Double
labelHelper n i 
  | i == n = 1.0
  | otherwise = 0.0

readLabel :: String -> Vector Double
readLabel label = 
  let n = read label
  in generate 10 (labelHelper n)

