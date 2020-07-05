module Math 
(
mult,
dot,
outer,
gaussDouble
) where

import System.Random
import System.Environment
import Control.Applicative
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Matrix as M
import qualified Data.Vector as V

-- Ax = b, typical matrix times vector implementation yielding a vector
mult :: (Num a) => Matrix a -> Vector a -> Vector a
mult m x = V.fromList [ x `dot` M.getRow i m | i <- [1 .. M.nrows m] ]


-- dot product between two vectors
dot :: (Num a) => Vector a -> Vector a -> a
dot x y = V.sum $ V.zipWith (*) x y

-- Outer product between a b is a * transpose(b)
outer :: (Num a) => Vector a -> Vector a -> Matrix a
outer u v = let outerProduct = pure (*) <*> u <*> v
            in M.fromList (length u) (length v) $ V.toList(outerProduct)

-- Simple (though probably not the best) normal distribution based on the Box-Muller transform
-- From Get a Brain Neural Network
gaussDouble :: Double -> IO Double
gaussDouble stdev = do
    x1 <- randomIO
    x2 <- randomIO
    return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)