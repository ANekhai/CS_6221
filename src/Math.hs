module Math 
(
mult,
dot,
relu,
sigmoid,
softMax,
convolve,
maxPool,
averagePool,
gaussDouble
) where

import System.Random
import System.Environment
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


-- typical nonlinear activation function for ML purposes which eliminates negative values
relu :: (Ord a, Num a) => Vector a -> Vector a
relu = V.map f
    where f x
            | x > 0 = x
            | otherwise = 0 


-- Another typically used nonlinear activation function for ML
-- helps convert to values between 0 and 1 which is useful for generating probabilities
sigmoid :: (Floating a) => Vector a -> Vector a
sigmoid = V.map f
    where 
        f x = 1 / (1 + exp (-x))


-- one last activation function, used for the last layer of a CNN
softMax :: (Floating a) => Vector a -> Vector a
softMax v = let normalizer = V.sum $ V.map exp v
            in V.map (/normalizer) $ V.map exp v


pad :: (Num a) => Int -> Matrix a -> Matrix a
pad size m = undefined


type Filter = Matrix

convolve :: (Floating a) => Filter a -> Matrix a -> Matrix a
convolve = undefined


maxPool :: (Num a, Ord a) => Matrix a -> Matrix a
maxPool = undefined

averagePool :: (Floating a) => Matrix a -> Matrix a
averagePool = undefined

-- Simple (though probably not the best) normal distribution based on the Box-Muller transform
-- From Get a Brain Neural Network
gaussDouble :: Double -> IO Double
gaussDouble stdev = do
    x1 <- randomIO
    x2 <- randomIO
    return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)