{-# LANGUAGE TypeSynonymInstances #-}

module ImageToVector (
  to2DMatrix,
  toWriteImage
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
import Graphics.Image.Interface.Vector
import Graphics.Image.ColorSpace
import qualified Graphics.Image.Interface as Interface
import Data.Word (Word8)
import qualified Data.Matrix as M
import System.FilePath.Posix (splitExtension)


-- takes an image filepath and outputs a 2D matrix
to2DMatrix :: FilePath  -> (Int, Int) -> IO (Maybe (M.Matrix Int)) 
to2DMatrix fp (dim1, dim2)= do
    eimg <- I.readImageY VS fp 
    let new_res :: Interface.Image VS I.Y Word8
        new_res = I.resize Bilinear Edge  (dim1, dim2) $ Interface.map conv eimg
    let rle = M.fromLists $ pixelToInt $ toJPImageY8 new_res
    return $ Just rle

-- takes an input and output filepaths and writes an image on the given path
-- using the similar processing as the 'to2DMarix' so you can see what the matrix looks like
-- only difference is that the this function outputs with Pixels in Double precision
-- whereas the 'to2DMatrix' function outputs in Word8 precision (0-255 value for each pixel)
toWriteImage :: FilePath -> FilePath -> (Int, Int) -> IO () 
toWriteImage fp fpout (dim1, dim2)= do
    eimg <- I.readImageY VS fp 
    let new_res :: Interface.Image VS I.Y Double
        new_res = I.resize Bilinear Edge (dim1, dim2) eimg
    let (name, _) = splitExtension fp
    I.writeImage fpout new_res

-- convert image pixels from Double to Word8 using Functor
conv :: Interface.Pixel I.Y Double -> Interface.Pixel I.Y Word8 
conv = fmap Interface.toWord8

        
-- convert Pixel8 image to a 2-d matrix of integers
pixelToInt :: Image Pixel8 -> [[Int]]
pixelToInt =
  map reverse . reverse .  snd . pixelFold -- because of the direction pixelFold works in, and the direction
    (\(lastY, ps:pss) x y p ->             -- you add things to lists, reverse and map reverse are necessary
      if y == lastY                        -- to make the output not mirrored horizontaly and vertically
        then (y, (fromIntegral p:ps):pss)
        else (y, [fromIntegral p]:ps:pss))
    (0,[[]])