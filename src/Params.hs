module Params
(
    Parameters (..),
    Parameter,
    addParameter
) where

-- TODO: Need to revamp this to be more monadic
-- Will need two files to specify a model, one is a file that goes through the structure: 
--      inDim outDim activation function
--
-- Second is a weights file that can be read in or printed out to once the model is done running

data Parameters = Parameters {mode :: Maybe String, modelFile :: Maybe FilePath, batches :: Maybe Int,
                          epochs :: Maybe Int, directory :: Maybe FilePath, help :: Maybe Bool}

type Parameter = (String, String)

addParameter :: Parameters -> Parameter -> Parameters
addParameter params (flag, x)
    | flag == "-h" = Parameters {mode = mode params, modelFile = modelFile params, batches = batches params,
                                 epochs = epochs params, directory = directory params, help = Just True}
    | flag == "-m" = Parameters {mode = Just x, modelFile = modelFile params, batches = batches params,
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-f" = Parameters {mode = mode params, modelFile = Just x, batches = batches params,
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-b" = Parameters {mode = mode params, modelFile = modelFile params, batches = Just $ read x,
                                 epochs = epochs params, directory = directory params, help = help params}
    | flag == "-e" = Parameters {mode = mode params, modelFile = modelFile params, batches = batches params,
                                 epochs = Just $ read x, directory = directory params, help = help params}
    | flag == "-d" = Parameters {mode = mode params, modelFile = modelFile params, batches = batches params,
                                 epochs = epochs params, directory = Just x, help = help params}
    | otherwise = params

parseArgs :: [String] -> Parameters -> IO Parameters
parseArgs [] params = return params
parseArgs (flag : x : xs) params = do
    let added_params = addParameter params (flag, x)
        next = if flag == "-h" then x:xs else xs
    parseArgs next added_params >>= return
parseArgs (flag:xs) params = do
    let added_params = addParameter params (flag, "")
    parseArgs xs added_params