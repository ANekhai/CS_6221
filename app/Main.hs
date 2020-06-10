{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImageToVector

main :: IO ()
main = do
	toRGBRaw "images/test_image.jpg"
