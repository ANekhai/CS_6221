module Main where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath.Posix ((</>))
import Data.List (isPrefixOf)
import System.Random.Shuffle (shuffleM)

import Model
import Math
import Train
import Utils
import Image
import Function
import Data.Vector (Vector, maxIndex)

main :: IO ()
main = do
    (runType:fps) <- getArgs --TODO: Implement a monadic way to check parameters
    if runType == "train"
        then trainAndTest fps
        else 
            if runType == "run"
                then evaluateImage fps
                else
                    error ("Please either specify train or run as an execution mode with proper parameters")

    return ()

evaluateImage :: [FilePath] -> IO ()
evaluateImage (layerFile:weightsFile:imageFile:[]) = do
    model <- fromFiles layerFile weightsFile
    predict model imageFile >>= putStrLn
    return ()
evaluateImage _ = do
    putStrLn ("Arguments were not correct. To run model please use: ")
    putStrLn ("image-exe run structure_file layer_weights_file image_file")
    putStrLn ("Please try again.")
    error ("Incorrect arguments")



trainAndTest :: [FilePath] -> IO ()
trainAndTest (layerSpec:trainDir:testDir:[]) = do
    layers <- getLayers layerSpec
    untrainedModel <- getRandomModel 784 layers

    let epochs = 1
        trainingRate = 0.01
    
    putStrLn ("Beginning Training on " ++ (show epochs) ++ " epoch(s). Writing every 100th loss value")

    trainedModel <- trainingPipeline epochs trainingRate trainDir untrainedModel

    putStrLn ("Training done. Writing model to file.")
    toFile "trainedSigmoid.cfg" trainedModel

    putStrLn ("Running testing.")
    percentCorrect <- testingPipeline trainedModel testDir

    putStrLn ("The percent correct is: " ++ (show percentCorrect))

    return ()
trainAndTest _ = do 
    putStrLn ("Arguments were not correct. To train model please use: ")
    putStrLn ("image-exe train structure_file training_dir_path testing_dir_path")
    putStrLn ("Please try again.")
    error ("Incorrect arguments")


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

-- reads in the 28x28 image from the first filepath to a 784 element vector, the second filepath is a number which is converted to a 
-- vector with 1.0 at that numbers index and 0.0 elsewhere.
-- Finally, the sigmoid activation function is applied to the image vector to prep for the Neural Net
fileToImage :: (Int, Int) -> (FilePath, FilePath) -> IO (Vector Double, Vector Double)
fileToImage dims (fp, n) = do
    let label = readLabel n
    image <- getImageVector fp dims
    return (sigmoid image, label)


trainingPipeline :: Int -> LearningRate -> FilePath -> Model Double -> IO (Model Double)
trainingPipeline epochs eta baseDir untrainedModel = do
    training_paths <- getImageFiles baseDir
    training_files <- mapM (fileToImage (28,28)) training_paths

    putStrLn "Training losses: "
    --TODO: Insert epoch training here maybe using the forM function when State monad is implemented for model
    shuffled <- shuffleM training_files
    let training_epochs = replicate epochs training_files
    shuffled <- mapM (shuffleM) training_epochs
    
    -- let (trainedModel, losses) = trainList eta squaredErrorLoss (concat shuffled) untrainedModel
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