{-# LANGUAGE RankNTypes #-}

module Train 
(
LossFn,
squaredErrorLoss,
logisticLoss,
LearningRate,
trainMany
) where

import Model
import Math
import Numeric.AD hiding (Scalar)
import Data.Vector (Vector)
import Data.Matrix (Matrix)
import qualified Data.Vector as V
import qualified Data.Matrix as M


--TODO: add batch processing (look into batch normalization), parallel or even GPU processing

type LossFn = forall a. Scalar a => Vector a -> Vector a -> a

squaredErrorLoss :: LossFn
squaredErrorLoss u v = V.sum (V.zipWith f u v) where f x y = (x - y) ^ 2

logisticLoss :: LossFn
logisticLoss u v = V.sum (V.zipWith f u v)
    where f x y = (-y) * log x - (1 - y) * log (1 - x)


-- This function tweaks the parameters we are training on
addParams :: (Num a) => Model a -> Model a -> Model a
addParams (Model layers) (Model diffs) = Model (zipWith addLayer layers diffs)
    where addLayer (Layer (LP m b) act) (Layer (LP dm db) _) = Layer (LP (M.elementwise (+) m dm) (V.zipWith (+) b db)) act


type LearningRate = Double

train :: LearningRate -> LossFn -> Model Double -> Vector Double ->
         Vector Double -> (Model Double, Double)
train rate lossFn model x y =
    let (loss, gradient) = grad' modelLoss model
    in (addParams model (fmap ((-rate)*) gradient), loss)
    where 
        modelLoss :: forall b. Scalar b => Model b -> b
        modelLoss model =
            lossFn (eval model (V.map realToFrac x)) (V.map realToFrac y)

trainMany :: LearningRate -> LossFn -> Model Double -> [(Vector Double, Vector Double)] ->
             (Model Double, [Double])
trainMany _ _ m [] = (m, [])
trainMany rate lossFn m ((x, y) : examples) = (m', loss : losses)
    where
        (m1, loss) = train rate lossFn m x y
        (m', losses) = trainMany rate lossFn m1 examples

