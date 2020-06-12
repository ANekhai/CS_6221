{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImageToVector

main :: IO ()
main = do
    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    writeFile ("images/test_image.txt") (show mat)

    -- if you want to write an image to see the resolution, use this: 
    toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)
