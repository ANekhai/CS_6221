{-# LANGUAGE TypeSynonymInstances #-}

module Image (
  get2DMatrix,
  outputImage,
  getImageVector
) where

import Codec.Picture         
import Codec.Picture.Types
import Control.Arrow
import Data.Ratio 
import Data.Monoid
import Graphics.Image.Processing
import qualified Graphics.Image as I
import Graphics.Image.IO
import Graphics.Image.IO.Formats
import qualified Graphics.Image.Interface.Vector as V
import Graphics.Image.ColorSpace
import qualified Graphics.Image.Interface as Interface
import Data.Word (Word8)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import System.FilePath.Posix (splitExtension, dropFileName)
import System.Random
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.Vector (Vector)
import Data.Matrix (flatten)


-- takes an image filepath and outputs a 2D matrix
--TODO: make sure this ouputs a matrix of Doubles not Ints
get2DMatrix :: FilePath  -> (Int, Int) -> IO (Maybe (M.Matrix Double)) 
get2DMatrix fp (dim1, dim2)= do
    eimg <- I.readImageY V.VS fp 
    let new_res :: Interface.Image V.VS I.Y Word8
        new_res = I.resize Bilinear Edge  (dim1, dim2) $ Interface.map conv eimg
    let rle = M.fromLists $ pixelToDouble $ toJPImageY8 new_res
    return $ Just rle

-- takes an input and output filepaths and writes an image on the given path
-- using the similar processing as the 'to2DMarix' so you can see what the matrix looks like
-- only difference is that the this function outputs with Pixels in Double precision
-- whereas the 'to2DMatrix' function outputs in Word8 precision (0-255 value for each pixel)
outputImage :: FilePath -> FilePath -> (Int, Int) -> IO () 
outputImage fp fpout (dim1, dim2)= do
    eimg <- I.readImageY V.VS fp 
    let new_res :: Interface.Image V.VS I.Y Double
        new_res = I.resize Bilinear Edge (dim1, dim2) eimg
    let (name, _) = splitExtension fp
    I.writeImage fpout new_res


getImageVector :: FilePath -> (Int, Int) -> IO (Vector Double)
getImageVector file dims= do
  (Just matrix) <- get2DMatrix file dims
  return (normalizeImage $ M.getMatrixAsVector matrix)

-- convert image pixels from Double to Word8 using Functor
conv :: Interface.Pixel I.Y Double -> Interface.Pixel I.Y Word8 
conv = fmap Interface.toWord8

  
-- convert Pixel8 image to a 2-d matrix of integers
pixelToDouble :: Image Pixel8 -> [[Double]]
pixelToDouble =
  map reverse . reverse .  snd . pixelFold -- because of the direction pixelFold works in, and the direction
    (\(lastY, ps:pss) x y p ->             -- you add things to lists, reverse and map reverse are necessary
      if y == lastY                        -- to make the output not mirrored horizontaly and vertically
        then (y, (fromIntegral p:ps):pss)
        else (y, [fromIntegral p]:ps:pss))
    (0,[[]])

normalizeImage :: Vector Double -> Vector Double
normalizeImage = V.map (/ 256)