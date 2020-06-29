module Utils (
    groupsOf,
    takeNth,
    toString,
    readLabel,
    shuffle
)
where

import Data.Vector (Vector, generate)
import Data.Map (Map, elems, singleton, insert, (!))
import System.Random


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


-- Algorithm to shuffle a list based on the Fisher Yates algorithm
-- From the HaskellWiki
-- O(nlogn)
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)


-- a wrapper for Fisher Yates to shuffle a list without managing the random generator
shuffle :: [a] -> IO [a]
shuffle list = do
    gen <- getStdGen
    return $ fst $ fisherYates gen list
