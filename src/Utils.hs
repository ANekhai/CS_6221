module Utils (
    groupsOf,
    toString,
    readLabel
)
where

import Data.Vector (Vector, generate)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

takeNth :: Int -> [a] -> [a]
takeNth n xs = [ x | i <- [1..(length xs)], x <- xs, i `mod` n == 0 ]

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