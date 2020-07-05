{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Model 
(
Model(..),
Layer(..),
LinearParams(..),
evalLinear,
Activation (..),
evalLayer,
layerAZ,
eval,
sigma',
layerWeights,
getRandomModel,
getModel,
getLayers,
getWeights,
toFile,
fromFiles
) where 

import Control.Monad
import System.IO
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import Data.List
import Data.String
import System.Random

import Math
import Utils
import Function

data LinearParams a = LP (Matrix a) (Vector a)
    deriving (Show, Functor, Foldable, Traversable)

evalLinear :: (Num a) => LinearParams a -> Vector a -> Vector a
evalLinear (LP m b) x = V.zipWith (+) (mult m x) b

newtype Activation = Activation (Function Double)

instance Show Activation where show _ = "Activation Function"

layerAZ :: Layer Double -> Vector Double -> (Vector Double, Vector Double)
layerAZ (Layer params (Activation (Fn {f=f}))) input =
    let z = evalLinear params input
        a = f z
    in (a, z)

-- Add pooling and convolution layers as expansion of this and pattern matching for evaluation
data Layer a = Layer (LinearParams a) Activation
    deriving (Show, Functor, Foldable, Traversable)

evalLayer :: Layer Double -> Vector Double -> Vector Double
evalLayer (Layer params (Activation (Fn {f=f}))) x = f $ evalLinear params x

newtype Model a = Model [Layer a]
    deriving (Show, Functor, Foldable, Traversable)

eval :: Model Double -> Vector Double -> Vector Double
eval (Model layers) x = foldl' (flip evalLayer) x layers

layers :: Model Double -> [Layer Double]
layers (Model layers) = layers

sigma' :: Layer Double -> (Vector Double -> Vector Double)
sigma' (Layer _ (Activation (Fn {f'=f'}))) = f'

layerWeights :: Layer Double -> Matrix Double
layerWeights (Layer (LP w _) _) = w

-- generate a random model
type LayerSpec = (Int, Activation)
type Dimension = Int

getRandomModel :: Dimension -> [LayerSpec] -> IO (Model Double)
getRandomModel inputDimension layerSpecs = do 
    let inputSizes = inputDimension : map fst layerSpecs 
    layers <- forM (zip inputSizes layerSpecs) $ \(m, (n, activation)) -> do
        weights <- fmap (M.fromList n m) $ replicateM (m * n) $ gaussDouble 1
        bias <- fmap V.fromList $ replicateM n $ gaussDouble 1
        -- let bias = V.fromList $ replicate n 1.0    -- specifically for MNIST relu net, found that biases starting at 1 is good for training a relu net
        return (Layer (LP weights bias) activation)
    return (Model layers)

getModel :: [LayerSpec] -> [(Matrix Double, Vector Double)] -> IO (Model Double)
getModel layerSpec weights = do
    let layers = foldr layerFunc [] $ zip layerSpec weights
    return (Model layers)
    where
        layerFunc ((_, activation), (w, b)) acc =
            (Layer (LP w b) activation) : acc


--TODO: Make these monadically better with error handling and optional types

-- reads in a file with matrix weights for the model
-- Format of file it reads is: xDim yDim Matrix Bias all on a single line
getWeights :: FilePath -> IO [(Matrix Double, Vector Double)] 
getWeights file = do
    weightString <- readFile file
    return $ foldr f [] $ map words $ lines weightString
        where f (x:y:xs) acc =  
                let m = read x
                    n = read y
                    values = map read xs
                    weights = M.fromList m n $ take (m*n) values
                    bias = V.fromList $ drop (m*n) values
                in  (weights, bias) : acc

-- Converts a file to a NN pipeline
-- Expects each layer to be a separate line with 
getLayers :: FilePath -> IO [LayerSpec]
getLayers file = do
    layerString <- readFile file
    return $ foldr f [] $ map words $ lines layerString
    where 
        f (n:act) acc = ((read n), (getActivation $ head act)) : acc


getActivation :: String -> Activation
getActivation name
    | name == "relu" = Activation reluFn
    | name == "softmax" = Activation softMaxFn
    -- | name == "maxpool" = Activation maxPool
    -- | name == "averagepool" = Activation averagePool
    -- | name == "convolution" = Activation convolve
    | otherwise = Activation sigmoidFn



fromFiles :: FilePath -> FilePath -> IO (Model Double)
fromFiles structureF weightF = do
    layers <- getLayers structureF
    weights <- getWeights weightF
    getModel layers weights
    
    
toFile :: FilePath -> Model Double -> IO ()
toFile file model = do
    handle <- openFile file WriteMode
    let model_state = getInternals $ layers model
    hPutStr handle (unlines $ foldr f [] model_state)
    hClose handle
    where f (a, b) acc = (unwords $ ((show $ M.nrows a): (show $ M.ncols a) : (toString $ M.toList a) : (toString $ V.toList b) : [])) : acc


getInternals :: [Layer Double] -> [(Matrix Double, Vector Double)]
getInternals [] = []
getInternals ((Layer (LP m b) _) : rest) =
    (m,b) : getInternals rest
