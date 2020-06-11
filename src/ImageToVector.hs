{-# LANGUAGE TypeSynonymInstances #-}

module ImageToVector (
to2DMatrix
) where

import Codec.Picture         
import Codec.Picture.Types
import Control.Arrow
import Data.Ratio 
import Data.Monoid
import Graphics.Image.Processing
-- import qualified Graphics.Image as I
import qualified Data.Matrix as M
import System.FilePath.Posix (splitExtension)

-- take image input with the filepath given as a DynamicImage type
to2DMatrix :: FilePath -> IO ()
to2DMatrix fp = do
    imagedyn <- readImage fp
    case imagedyn of
      Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
      Right dynimg -> do
        let rle = twoDToMatrix $ pixelToInt $ greyscaleImage dynimg
        let (name, _) = splitExtension fp
        writeFile (name ++ ".txt") (show rle)

-- changeResolution :: Border () -> (Int, Int) -> (Image arr I.RGB Double) -> (Image arr cs e)
-- changeResolution border (dim1, dim2) img = I.resize border (dim1, dim2) img

-- convert DynamicImage to a Pixel8 image
greyscaleImage :: DynamicImage -> Image Pixel8
greyscaleImage = convertRGB8 >>> pixelMap greyscalePixel

-- convert PixelsRGB8 image to Pixel8 image
greyscalePixel :: PixelRGB8 -> Pixel8
greyscalePixel (PixelRGB8 r g b) = round (wr + wg + wb)
  where wr = toRational r * (3 % 10)
        wg = toRational g * (59 % 100)
        wb = toRational b * (11 % 100)
        
-- convert Pixel8 image to a 2-d matrix of integers
pixelToInt :: Image Pixel8 -> [[Int]]
pixelToInt =
  map reverse . reverse .  snd . pixelFold -- because of the direction pixelFold works in, and the direction
    (\(lastY, ps:pss) x y p ->             -- you add things to lists, reverse and map reverse are necessary
      if y == lastY                        -- to make the output not mirrored horizontaly and vertically
        then (y, (fromIntegral p:ps):pss)
        else (y, [fromIntegral p]:ps:pss))
    (0,[[]])

-- converts list of lists to Data.Matrix type Matrix
twoDToMatrix :: [[Int]] -> M.Matrix Int
twoDToMatrix lists = M.fromLists lists



--- draft code that tries to combine Graphics.Image and Codec.Picture by inputting, 
-- then outputting, then inputting an image. Does not work 

-- to2DMatrix :: FilePath -> IO() -> (Int, Int) -> Border(String) -> IO () -> IO()
-- to2DMatrix fp bor dim1 dim2 = do
--     image_rgb <- I.readImageRGB VU fp
--     case image_rgb of 
--       Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
--       Right dynimg -> do
--         let dimensions = (dim1, dim2)
--         new_image <- decodeImage $ I.resize bor dimensions dynimg
--         case new_image of
--           Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
--           Right image -> do
--             let rle = twoDToMatrix $ pixelToInt $ greyscaleImage image
--             let (name, _) = splitExtension fp
--             writeFile (name ++ ".txt") (show rle)