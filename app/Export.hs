{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

module Export where
import Foreign.HaPy
import Main

initHaPy

pythonExport 'predict
