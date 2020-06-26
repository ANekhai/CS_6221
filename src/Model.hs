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
Scalar,
Activation,
evalLayer,
eval,
getRandomModel
) where 

import Control.Monad
import System.IO
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import Data.List
import Data.String

import Math
import Utils

data LinearParams a = LP (Matrix a) (Vector a)
    deriving (Show, Functor, Foldable, Traversable)

evalLinear :: (Num a) => LinearParams a -> Vector a -> Vector a
evalLinear (LP m b) x = V.zipWith (+) (mult m x) b


type Scalar a = (Eq a, Ord a, Floating a, RealFrac a)

newtype Activation = Activation (forall a. Scalar a => Vector a -> Vector a)

instance Show Activation where show _ = "Activation Function"


data Layer a = Layer (LinearParams a) Activation
    deriving (Show, Functor, Foldable, Traversable)

evalLayer :: (Scalar a) => Layer a -> Vector a -> Vector a
evalLayer (Layer params (Activation f)) = f . evalLinear params

newtype Model a = Model [Layer a]
    deriving (Show, Functor, Foldable, Traversable)

eval :: (Scalar a) => Model a -> Vector a -> Vector a
eval (Model layers) x = foldl' (flip evalLayer) x layers

layers :: Model Double -> [Layer Double]
layers (Model layers) = layers

-- generate a random model
type LayerSpec = (Int, Activation)
type Dimension = Int

getRandomModel :: Dimension -> [LayerSpec] -> IO (Model Double)
getRandomModel inputDimension layerSpecs = do 
    let inputSizes = inputDimension : map fst layerSpecs 
    layers <- forM (zip inputSizes layerSpecs) $ \(m, (n, activation)) -> do
        weights <- fmap (M.fromList n m) $ replicateM (m * n) $ gaussDouble 1 --TODO: change this so it generates weights ~N(0,1)
        bias <- fmap V.fromList $ replicateM n $ gaussDouble 1
        return (Layer (LP weights bias) activation)
    return (Model layers)
--TODO: Implement convolution and pooling layers for a CNN



--TODO: Make these monadically better with error handling

-- reads in a file with matrix weights for the model
-- Format of file it reads is: xDim yDim Matrix Bias all on a single line
getWeights :: FilePath -> IO [(Matrix Double, Vector Double)] 
getWeights file = do
    handle <- openFile file ReadMode  --TODO: check if file is there
    contents <- hGetContents handle
    hClose handle
    return ( foldr f [] (map words $ lines contents) )
        where f (x:y:xs) acc =  let 
                                m = read x
                                n = read y
                                values = map read xs
                                weights = M.fromList m n $ take (m*n) values
                                bias = V.fromList $ drop (m*n) values
                                in (weights, bias) : acc

-- Converts a file to a NN pipeline
-- Expects each layer to be a separate line with 
getLayers :: FilePath -> IO [LayerSpec]
getLayers file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    hClose handle
    return ( foldr f [] (map words $ lines contents) )
        where 
            f (n:act) acc = ((read n), (getActivation $ head act)) : acc

getActivation :: String -> Activation
getActivation name
    | name == "relu" = Activation relu
    | name == "softmax" = Activation softMax
    -- | name == "maxpool" = Activation maxPool
    -- | name == "averagepool" = Activation averagePool
    -- | name == "convolution" = Activation convolve
    | otherwise = Activation sigmoid



fromFiles :: FilePath -> FilePath -> IO (Model Double)
fromFiles structure weights = undefined
    
    
toFile :: Model Double -> FilePath -> IO ()
toFile model file = do
    handle <- openFile file WriteMode
    let model_state = getInternals $ layers model
    hPutStr handle (unlines $ foldr f [] model_state)
    hClose handle
    where f (a, b) acc = (unwords $ ((show $ M.nrows a): (show $ M.ncols a) : (toString $ M.toList a) : (toString $ V.toList b) : [])) : acc


getInternals :: [Layer Double] -> [(Matrix Double, Vector Double)]
getInternals [] = []
getInternals ((Layer (LP m b) _) : rest) =
    (m,b) : getInternals rest
