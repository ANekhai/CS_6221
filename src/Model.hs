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
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import Data.List
import Math

data LinearParams a = LP (Matrix a) (Vector a)
    deriving (Show, Functor, Foldable, Traversable)

evalLinear :: (Num a) => LinearParams a -> Vector a -> Vector a
evalLinear (LP m b) x = V.zipWith (+) (mult m x) b


type Scalar a = (Eq a, Ord a, Floating a, RealFrac a)

newtype Activation = Activation (forall a. Scalar a => Vector a -> Vector a)

instance Show Activation where show _ = "Activation"


data Layer a = Layer (LinearParams a) Activation
    deriving (Show, Functor, Foldable, Traversable)

evalLayer :: (Scalar a) => Layer a -> Vector a -> Vector a
evalLayer (Layer params (Activation f)) = f . evalLinear params

newtype Model a = Model [Layer a]
    deriving (Show, Functor, Foldable, Traversable)

eval :: (Scalar a) => Model a -> Vector a -> Vector a
eval (Model layers) x = foldl' (flip evalLayer) x layers

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

fromFile :: FilePath
fromFile = undefined
