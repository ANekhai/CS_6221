import Data.Matrix (Matrix)
import Data.Vector (Vector)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Math

module Model (
    Model,
    Layer
    )
    where 


data LinearParams a = LP (Matrix a) (Vector a)
    deriving (Show, Functor, Foldable, Traversable)

evalLinear :: Num a => LinearParams a -> Vector a -> Vector a
evalLinear (LP m b) x = V.zipWith (+) (mult m x) b



newtype Model a = Model []