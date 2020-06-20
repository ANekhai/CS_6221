module Main where


import ImageToVector
import System.FilePath.Posix (splitExtension)

main :: IO ()
main = do

    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    -- mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    -- writeFile "images/test_image.txt" (show mat)

    -- if you want to writesee the resolution, use this: 
    -- toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)

    let train_filepath = 'MNIST_data/training'
    let chunk_size = 10

    list_fp <- traverseDir train_filepath "None"
    shuff_list <- take chunk_size $ shuffle list_fp

    train_image = do
        vectors = fmap labelAndTrain shuff_list 