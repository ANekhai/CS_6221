module Function (
    Function (..),
    LossFunction (..),
    calculateLoss,
    squaredErrorLoss,
    -- logisticLoss,
    reluFn,
    sigmoidFn,
    softMaxFn,
    convFn,
    sigmoid -- to apply to images before ML
) where

import Data.Vector (Vector)
import Data.Matrix (Matrix)
import qualified Data.Vector as V
import qualified Data.Matrix as M

data Function a = Fn {f::(Vector a -> Vector a), f'::(Vector a -> Vector a)}
data LossFunction a = LFn {lossFn::(Vector a -> Vector a -> a), dF::(Vector a -> Vector a -> Vector a) }

calculateLoss :: LossFunction Double -> Vector Double -> Vector Double -> Double
calculateLoss (LFn {lossFn=lossFn}) u = lossFn u

squaredErrorLoss :: LossFunction Double
squaredErrorLoss = LFn { lossFn = squaredError, dF = dSquaredError}

-- TODO: Figure out the derivative of this function
-- logisticLoss :: LossFunction Double
-- logisticLoss u v = V.sum (V.zipWith f u v)
--     where f x y = (-y) * log x - (1 - y) * log (1 - x)

reluFn :: Function Double
reluFn = Fn {f = relu, f' = relu'}

sigmoidFn :: Function Double
sigmoidFn = Fn {f = sigmoid, f' = sigmoid'}

softMaxFn :: Function Double
softMaxFn = Fn {f = softMax, f' = softMax}

convFn :: Function Double
convFn = undefined


-- typical nonlinear activation function for ML purposes which eliminates negative values
relu :: (Ord a, Num a) => Vector a -> Vector a
relu = V.map f
    where f x
            | x > 0 = x
            | otherwise = 0


relu' :: (Ord a, Num a) => Vector a -> Vector a
relu' = V.map f
    where f x
            | x > 0 = 1
            | otherwise = 0


-- Another typically used nonlinear activation function for ML
-- helps convert to values between 0 and 1 which is useful for generating probabilities
sigmoid :: (Floating a) => Vector a -> Vector a
sigmoid = V.map f
    where 
        f x = 1 / (1 + exp (-x))

sigmoid' :: (Floating a) => Vector a -> Vector a
sigmoid' = V.map f
    where
        f x = exp (-x) / ((1 + exp (-x)) ^ 2)

-- one last activation function, used for the last layer of a CNN
softMax :: (Floating a) => Vector a -> Vector a
softMax v = let normalizer = V.sum $ V.map exp v
            in V.map (/normalizer) $ V.map exp v

softMax' :: (Floating a) => Vector a -> Vector a
softMax' = undefined --TODO: fill in the undefined portion 
                     -- for i == j : this is softMax(x_j) * (1 - softMax(x_i)
                     -- otherwise  : - softMax(x_j) * softMax(x_i))

squaredError :: Vector Double -> Vector Double -> Double
squaredError a y = (/2) . sum $ V.map (^2) (V.zipWith (-) a y)

-- dSquaredError :: Vector Double -> Vector Double -> Vector Double
-- dSquaredError a y = V.zipWith (-) a y

dSquaredError :: Vector Double -> Vector Double -> Vector Double
dSquaredError a y = V.zipWith (dSECost) a y

-- A modified squared error function
dSECost :: Double -> Double -> Double
dSECost a y 
--    | y == 1 && a >= y = 0
    | otherwise        = a - y


pad :: (Num a) => Int -> Matrix a -> Matrix a
pad size m = undefined


type Filter = Matrix

convolve :: (Floating a) => Filter a -> Matrix a -> Matrix a
convolve = undefined


maxPool :: (Num a, Ord a) => Matrix a -> Matrix a
maxPool = undefined

averagePool :: (Floating a) => Matrix a -> Matrix a
averagePool = undefined