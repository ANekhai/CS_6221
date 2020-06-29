{-# LANGUAGE RankNTypes #-}

module Train 
(
LossFn,
squaredErrorLoss,
logisticLoss,
LearningRate,
train,
trainList,
runBatchlessTraining
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
train eta lossFunction model x y =
    let (loss, gradient) = grad' modelLoss model
    in (addParams model (fmap ((- eta) *) gradient), loss)
        where 
        modelLoss :: forall b. Scalar b => Model b -> b
        modelLoss model =
            lossFunction (eval model (V.map realToFrac x)) (V.map realToFrac y)


trainList :: LearningRate -> LossFn -> Model Double -> [(Vector Double, Vector Double)] ->
             (Model Double, [Double])
trainList _ _ m [] = (m, [])
trainList eta lossFunction m ((x, y) : xs) = (m', loss : losses)
    where
        (new_m, loss) = train eta lossFunction m x y
        (m', losses) = trainList eta lossFunction new_m xs


runBatchlessTraining :: LearningRate -> LossFn -> [[(Vector Double, Vector Double)]] -> (Model Double, [Double]) -> (Model Double, [Double])
runBatchlessTraining _ _ [] (model, _ ) = (model, [])
runBatchlessTraining eta lossFunction (epoch : remaining) (model, losses) = (new_model, concat $ new_losses : losses : [])
    where
    (new_model, new_losses) = runBatchlessTraining eta lossFunction remaining $ trainList eta lossFunction model epoch
    