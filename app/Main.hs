module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath.Posix ((</>))

import Params
import Model
import Math
import Train
import Utils
import Image
import Data.Vector (Vector)

main :: IO ()
main = do

    -- if you just want the 2D matrix output from the image
    -- define the number of pixels (or dimensions) of the image you want
    -- mat <- to2DMatrix "images/test_image.jpg" (100, 100)
    -- writeFile "images/test_image.txt" (show mat)

    -- if you want to writesee the resolution, use this:
    -- toWriteImage "images/test_image.jpg" "images/test_image_output.jpg" (100, 100)

    -- args <- getArgs
    -- let empty_params = Parameters {mode = Nothing, modelFile = Nothing, batches = Nothing, epochs = Nothing,
    --                                directory = Nothing, help = Nothing}
    -- parameters <- parseArgs args empty_params

    --TODO: Implement a monadic way to check parameters
        -- Check if directory, batches, epochs defined for training
        -- read input model from infile for running the model
    
    -- TODO: Rewrite this hardcoded training algorithm
    -- Setting up algorithm
    let epochs = 5
        layers = [(30, relu), (30, sigmoid), (10, softMax)]
        base_dir = "~/MNISTtrain"
    training_paths <- getTrainingFiles base_dir
    training_set <- mapM_ (\(path, n) -> do 
            let l = readLabel n
            img <- getImageVector base_dir (28,28)
            return (img, l)
            ) training_paths


    return ()


parseArgs :: [String] -> Parameters -> IO Parameters
parseArgs [] params = return params
parseArgs (flag : x : xs) params = do
    let added_params = addParameter params (flag, x)
        next = if flag == "-h" then x:xs else xs
    parseArgs next added_params >>= return
parseArgs (flag:xs) params = do
    let added_params = addParameter params (flag, "")
    parseArgs xs added_params


getTrainingFiles :: FilePath -> IO [(FilePath, FilePath)]
getTrainingFiles training_dir = do
    labels <- listDirectory training_dir -- should be a list ['0', '1', '2' ... '9']
    let subdirs = map (training_dir </> ) labels
        labelled_subdirs = zip subdirs labels

    filepaths <- forM labelled_subdirs (\(dp, label) -> do
        contents <- listDirectory dp
        let fps = map (dp </>) contents
        return $ zip fps $ repeat label
        )
   -- training_files <- getFiles labelled_subdirs
    return $ concat filepaths

--TODO: can add code to log epoch and batch used monadically

