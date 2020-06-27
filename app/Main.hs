{-# LANGUAGE OverloadedStrings #-}

module Main where

import ImageToVector
import Web.Scotty
import System.FilePath.Posix (splitExtension)
import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = do
    server 

server = scotty 5000 $ do

    -- get "/" $ text "foobar"

    -- endpoint for our prediction call from Flask app
    post "/prediction" $ do  
        -- get parameter filepath from the POST call
        fp <-  param "filepath"
        text $ decodeUtf8 fp

    get "/" $ text "Server is running.."

    --DRAFT - what this would look like 
    -- post "/prediction" $ do  
    --     fp <-  param "filepath"
    --     text $ predict $ transpose $ flatten $ getImageVector $ TL.decodeUtf8 fp
    -- where 'predict' function type signature is Vector -> [Int] or similar and 
    -- returns the prediction