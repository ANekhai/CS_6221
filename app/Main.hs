module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath.Posix ((</>))
import Data.List (isPrefixOf)
import System.Random.Shuffle (shuffleM)

import Params
import Model
import Math
import Train
import Utils
import Image
import Function
import Data.Vector (Vector, maxIndex)

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
    let epochs = 1
        layers = [(30, Activation sigmoidFn), (10, Activation sigmoidFn)]
        baseDir = "/home/anton/MNIST_test/"
        -- baseDir = "/home/anton/MNIST_train"

    model <- fromFiles "/home/anton/MNIST_config/relu.cfg" "/home/anton/MNIST_config/trainedRelu.cfg"
    percentCorrect <- testingPipeline model baseDir

    putStrLn ("The percent correct is: " ++ (show percentCorrect))

    -- untrainedModel <- getRandomModel 784 layers

    -- putStrLn ("Beginning Training on " ++ (show epochs) ++ " epochs.")
    
    -- trainedModel <- trainingPipeline epochs 0.1 baseDir untrainedModel
    
    -- toFile "/home/anton/trainedSigmoid.cfg" trainedModel
    
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


getImageFiles :: FilePath -> IO [(FilePath, FilePath)]
getImageFiles training_dir = do
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


trainingPipeline :: Int -> LearningRate -> FilePath -> Model Double -> IO (Model Double)
trainingPipeline epochs eta baseDir untrainedModel = do
    training_paths <- getImageFiles baseDir
    training_files <- mapM (fileToImage (28,28)) training_paths

    putStrLn "Training losses: "
    --TODO: Insert epoch training here maybe using the forM function
    shuffled <- shuffleM training_files
    let training_epochs = replicate epochs training_files
    shuffled <- mapM (shuffleM) training_epochs
    
    let (trainedModel, losses) = trainList eta squaredErrorLoss (take 10000 $ concat shuffled) untrainedModel
    
    mapM_ putStrLn (map show $ takeNth 100 losses)
    -- mapM_ putStrLn (map show losses)

    return trainedModel

testingPipeline :: Model Double -> FilePath -> IO Double
testingPipeline model baseDir = do
    testing_paths <- getImageFiles baseDir
    testing_files <- mapM (fileToImage (28,28)) testing_paths

    results <- mapM (tupleFeed model) testing_files

    let counts = foldl checkAnswer [] results

    return $ sum counts / (fromIntegral $ length counts)

    where
        checkAnswer acc (result, label) = 
            let prediction = getPrediction result
                known = getPrediction label
            in if prediction == known then (1.0 : acc) else (0.0 : acc)




-- helper function for testing pipeline
tupleFeed :: Model Double -> (Vector Double, Vector Double) -> IO (Vector Double, Vector Double)
tupleFeed model (input, label) = do
    let result = eval model input
    return (result, label)


getPrediction :: Vector Double -> Int
getPrediction = maxIndex

predict :: Model Double -> FilePath -> IO String
predict model ifp = do 
    image <- getImageVector ifp (28,28)
    let prediction = getPrediction $ eval model image
    return $ show prediction 