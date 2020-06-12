{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImageToVector

main :: IO ()
main = do
    -- provide filepath and dimensions of new iage
    to2DMatrix "images/test_image.jpg" "images/test_image_output.jpg" Edge (25, 25)
-- to2DMatrix "images/test_image.jpg" Bilinear 25 25
