{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import qualified Graphics.Image as I
import Graphics.Image.Processing (rotate180)
import Graphics.Image.IO.Formats
import qualified Graphics.Image.Interface as IM
import qualified Graphics.Image.Interface.Repa as R
import Graphics.Image.ColorSpace
import Graphics.Image.Types
type BasicImage = (Image VS YCbCr Word8)

main :: IO ()
main = do
  img <- I.readImageExact' VU "image.jpg" :: IO(BasicImage)
  case img of
    Left err -> do
    	-- putStrLn $ "Error loading image:" ++ path
      print err
      return Nothing
    Right rgb -> do
      return $ (Just rgb)

