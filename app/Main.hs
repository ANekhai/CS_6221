module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath.Posix ((</>))

main :: IO ()
main = do

    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    -- mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    -- writeFile "images/test_image.txt" (show mat)

    -- if you want to writesee the resolution, use this: 
    -- toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)

    args <- getArgs
    let empty_params = Parameters {mode = Nothing, infile = Nothing, batches = Nothing, epochs = Nothing,
                                   directory = Nothing, help = Nothing}
    parameters <- parseArgs args empty_params


    -- let train_filepath = "MNIST_data/training"


    

    --list_fp <- traverseDir train_filepath (=="None")
--    shuffled_list <- shuffle list_fp

    return ()

data Parameters = Parameters {mode :: Maybe String, infile :: Maybe FilePath, batches :: Maybe Int,
                          epochs :: Maybe Int, directory :: Maybe FilePath, help :: Maybe Bool}

type Parameter = (String, String)

addParameter :: Parameters -> Parameter -> Parameters
addParameter params (flag, x)
    | flag == "-h" = Parameters {mode = mode params, infile = infile params, batches = batches params, 
                                 epochs = epochs params, directory = directory params, help = Just True}
    | flag == "-m" = Parameters {mode = Just x, infile = infile params, batches = batches params, 
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-f" = Parameters {mode = mode params, infile = Just x, batches = batches params, 
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-b" = Parameters {mode = mode params, infile = infile params, batches = Just $ read x, 
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-e" = Parameters {mode = mode params, infile = infile params, batches = batches params, 
                                 epochs = Just $ read x, directory = directory params, help = help params}
    | flag == "-d" = Parameters {mode = mode params, infile = infile params, batches = batches params, 
                                 epochs = epochs params, directory = Just x, help = help params}                                                      
    | otherwise = error "Unknown parameter used"




parseArgs :: [String] -> Parameters -> IO Parameters
parseArgs [] params = return params
parseArgs (flag:x:xs) params = do
    let added = addParameter params (flag,x)
        next = if flag == "-h" then x:xs else xs
    parseArgs next added >>= return 
    

getTrainingFiles :: FilePath -> IO [(FilePath, FilePath)]
getTrainingFiles training_dir = do
    labels <- listDirectory training_dir -- should be a list ['0', '1', '2' ... '9']
    let subdirs = map (training_dir </> ) labels
        labelled_subdirs = zip labels subdirs
    
    filepaths <- forM labelled_subdirs (\(label, dp) -> do
        contents <- listDirectory dp
        let fps = map (dp </>) contents
        return (map ((,) label) fps)
        )
   -- training_files <- getFiles labelled_subdirs
    return $ concat filepaths

