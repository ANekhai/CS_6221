{-# LANGUAGE RankNTypes #-}

module Train 
(
LearningRate,
stochasticGD,
feedForward,
backpropagate,
getDeltas,
trainList
) where

import Model
import Math
import Function
import Data.Vector (Vector)
import Data.Matrix (Matrix)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.List as L


--TODO: add batch processing (look into batch normalization), parallel or even GPU processing

type LearningRate = Double

-- TODO: Think about the flexibility of this function so it can be purposed in batch training
-- SGD :: LearningRate -> LossFn -> Model Double -> Model Double
-- SGD eta lossFn model =

-- Takes the model and an input vector and returns all the internal weighted inputs z
-- and a = sigma(z) for all the layers
-- Note these are returned backwards, so the last a is first in the output
--TODO: Will need to change how this works for a convolutional neural network
feedForward :: Model Double -> Vector Double -> [(Vector Double, Vector Double)]
feedForward (Model (first:rest)) input = 
    let firstAZ = layerAZ first input
    in L.foldl' f [firstAZ] rest
    where
        f ((a,z): xs) layer = (layerAZ layer a) : (a,z) : xs

backpropagate :: Model Double -> Vector Double -> [Vector Double] -> [Vector Double]
backpropagate (Model layers) dCost (zLast : zs) = 
    let (lastLayer : rLayers) = reverse layers
        delta0 = V.zipWith (*) dCost ((sigma' lastLayer) zLast)
        firstBackprop = mult (M.transpose $ layerWeights lastLayer) delta0
    in tail $ foldl calculateDelta [firstBackprop, delta0] (zip rLayers zs) -- Note: need to take the tail as the head of the list will be delta1 * weights1
    where 
        calculateDelta (backPropDelta:rest) ((Layer (LP w _) (Activation (Fn {f'=f'}))), z) =
            let newDelta = V.zipWith (*) backPropDelta (f' z)
                nextBackprop = mult (M.transpose w) newDelta
            in  nextBackprop : newDelta : rest

-- Return the delta values for the model in the form of a new model
getDeltas :: LossFunction Double -> (Vector Double, Vector Double) -> Model Double -> ([Vector Double], [Vector Double])
getDeltas (LFn {dF=dF}) (x, y) model =
    let azs = feedForward model x
        (out : as) = map (fst) azs
        zs = map (snd) azs
        dCost = dF out y
        deltas = backpropagate model dCost zs
    in (deltas, reverse (out:as))


stochasticGD :: LearningRate -> LossFunction Double -> (Vector Double, Vector Double) -> Model Double -> (Model Double, Double)
stochasticGD eta lossFunc (x, y) model =
    let (deltas, as) = getDeltas lossFunc (x, y) model
        modelLoss = calculateLoss lossFunc y (head as)
        adjDeltas = map (V.map (* eta)) deltas
        parameterAdjustment = zip (zipWith outer adjDeltas (x:as))  adjDeltas -- TODO: Error comes with adding the input, as it is not passed through an activation function
        newModel = changeWeights parameterAdjustment model
    in  (newModel, modelLoss)

-- helper function for SGD
changeWeights :: [(Matrix Double, Vector Double)] -> Model Double -> Model Double
changeWeights deltas (Model layers) = 
    let newLayers = foldr f [] (zip layers deltas)
    in  Model newLayers
    where
        f ((Layer (LP w b) activation), (deltaW, deltaB)) acc =
            (Layer (LP (M.elementwise (-) w deltaW) (V.zipWith (-) b deltaB)) activation) : acc

            
trainList :: LearningRate -> LossFunction Double -> [(Vector Double, Vector Double)] -> Model Double -> (Model Double, [Double])
trainList _ _ [] model = (model, [])
trainList eta lossFunc ((x,y):xs) model = (trainedM, loss : losses)
    where
        (updatedM, loss) = stochasticGD eta lossFunc (x,y) model
        (trainedM, losses) = trainList eta lossFunc xs updatedM
    