module Main where


import ImageToVector
import System.Directory
import System.FilePath.Posix (isPathSeparator, (</>))

main :: IO ()
main = do

    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    -- mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    -- writeFile "images/test_image.txt" (show mat)

    -- if you want to writesee the resolution, use this: 
    -- toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)

    let train_filepath = "MNIST_data/training"
        chunk_size = 10

    sub_files <- listDirectory train_filepath -- should be a list ['0', '1', '2' ... '9']
    let 
        subdirs = map (train_filepath </> ) sub_files
        labelled_subdirs = zipWith (,) labels subdirs
        
        


    --list_fp <- traverseDir train_filepath (=="None")
--    shuffled_list <- shuffle list_fp

    return ()

    -- train_image = do
    --     vectors = fmap labelAndTrain shuffled_list 

    
-- shuffles a list
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))