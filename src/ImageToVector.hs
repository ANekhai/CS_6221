-- module Lib
--     ( someFunc
--     ) where

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

-- module Lib
-- ( loadImageToVector
-- ) where

-- import System.Environment
-- import qualified Graphics.Image as I
-- import Graphics.Image.Processing (rotate180)
-- import Graphics.Image.IO.Formats
-- import qualified Graphics.Image.Interface as IM
-- import qualified Graphics.Image.Interface.Repa as R
-- import Graphics.Image.ColorSpace
-- import Graphics.Image.Types
-- import Data.Word (Word8)
-- type BasicImage = (Image VS RGB Double)

-- loadImageToVector :: FilePath -> IO (Maybe BasicImage)
-- loadImageToVector path = do 
--   img <- I.readImage path :: IO(Either String BasicImage)
--   case img of 
-- 		Left err -> do
-- 			putStrLn $ "Error loading image."
-- 			print err
-- 			return Nothing
-- 		Right rgb -> do
-- 			return $ Just rgb 

{-# LANGUAGE TypeSynonymInstances #-}

module ImageToVector (
toRGBRaw
) where

import Codec.Picture         (readImage, pixelAt, PixelRGB8(..))
import Codec.Picture.Types
import System.FilePath.Posix (splitExtension)

toRGBRaw :: FilePath -> IO ()
toRGBRaw fp = do
    image <- readImage fp
    case image of
      Left _ -> putStrLn $ "Sorry, not a supported codec for " ++ fp
      Right dynimg -> do
        let imgrgba8 = fromDynamicImage dynimg
        let (name, _) = splitExtension fp
        writeFile (name ++ ".txt") (concat $ accumPixels imgrgba8)

accumPixels :: Image PixelRGBA8 -> [String]
accumPixels img@(Image w h _) = [ format (pixelAt img x y) x y | x <- [0..(w-1)], y <- [0..(h-1)]]
  where format (PixelRGBA8 r g b _) j k = "#" ++ show r ++ "$"
                                              ++ show g ++ "$"
                                              ++ show b ++ "$"
                                              ++ show j ++ "$"
                                              ++ show k ++ "*\n"


-- Copied from
-- See http://hackage.haskell.org/package/JuicyPixels-util-0.2/docs/Codec-Picture-RGBA8.html

class ToPixelRGBA8 a where
    toRGBA8 :: a -> PixelRGBA8

instance ToPixelRGBA8 Pixel8 where
    toRGBA8 b = PixelRGBA8 b b b 255

instance ToPixelRGBA8 PixelYA8 where
    toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
    toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
    toRGBA8 = id

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageY8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageYA8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img) = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img

-- end of Codec.Picture.RGBA8