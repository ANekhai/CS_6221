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
eval
) where 

import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Vector as V
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

--TODO: Implement convolution and pooling layers for a CNN
