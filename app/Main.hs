{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImageToVector
import System.FilePath.Posix (splitExtension)

main :: IO ()
main = do
    -- provide filepath and dimensions of new iage
    mat <- to2DMatrix "images/test_image.jpg"
    writeFile ("images/test_image.txt") (show mat)

    -- to2DMatrix "images/test_image2.jpg"
-- to2DMatrix "images/test_image.jpg" Bilinear 25 25
