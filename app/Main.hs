module Main where


import ImageToVector
import System.FilePath.Posix (splitExtension)

main :: IO ()
main = do

    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    writeFile "images/test_image.txt" (show mat)

    -- if you want to writesee the resolution, use this: 
    toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)