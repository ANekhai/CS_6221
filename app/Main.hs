{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImageToVector

main :: IO ()
main = do
    -- provide filepath and dimensions of new iage
    to2DMatrix "images/test_image.jpg"
    to2DMatrix "images/test_image2.jpg"
-- to2DMatrix "images/test_image.jpg" Bilinear 25 25
