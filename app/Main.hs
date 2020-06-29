module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath.Posix ((</>))
import Data.List (isPrefixOf)

import Params
import Model
import Math
import Train
import Utils
import Image
import Data.Vector (Vector)

main :: IO ()
main = do
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
        layers = [(30, Activation sigmoid), (10, Activation relu)]
        base_dir = "/home/anton/MNIST_data/"
    training_paths <- getTrainingFiles base_dir
    training_files <- mapM (fileToImage (28,28)) training_paths
    shuffled <- shuffle training_files

    untrainedModel <- getRandomModel 784 layers
    toFile "/home/anton/premodel.cfg" untrainedModel

    putStrLn "Beginning Training."
    
    let (trainedModel, losses) = trainList 0.00000000000000000000000000000000000001 squaredErrorLoss untrainedModel (take 1000 shuffled)
    
    putStrLn "Training losses: "
    -- mapM_ putStrLn (map show $ takeNth 5 losses)
    mapM_ putStrLn (map show losses)

    toFile "/home/anton/postmodel.cfg" trainedModel
    
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
    subfiles <- listDirectory training_dir -- should be a list ['0', '1', '2' ... '9']
    let labels = filter (not . isPrefixOf ".") subfiles
    let subdirs = map (training_dir </> ) labels
        labelled_subdirs = zip subdirs labels

    filepaths <- forM labelled_subdirs (\(dp, label) -> do
        contents <- listDirectory dp
        let images = filter (not . isPrefixOf ".") contents
            fps = map (dp </>) images
        return $ zip fps $ repeat label
        )
   -- training_files <- getFiles labelled_subdirs
    return $ concat filepaths


fileToImage :: (Int, Int) -> (FilePath, FilePath) -> IO (Vector Double, Vector Double)
fileToImage dims (fp, n) = do
    let label = readLabel n
    image <- getImageVector fp dims
    return (image, label)
    
