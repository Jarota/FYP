{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.GLUT
import Data.IORef

import Parsers
import Display
import Bindings
import Types

import Data.Scientific
import Data.Void
import Data.Text (Text)
import Text.Megaparsec

main :: IO ()
main = do
    dataFile <- readFile "/home/jim/college/fyp/data.txt"
    print . words $ dataFile


    let res = runParser p "" "123"
    print res

{-
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "DataVis"

    let visualisation = Vis (Scatter3D "TEST" [(XYZ ([1, 3, 4, 5, 6, 7, 8, 9], [2, 1, 7, 3, 4, 8, 6, 9], [5, 2, 8, 3, 4, 1, 6, 9]))]) ([Types.Black, Types.Red] :: ColourScheme)
    vis <- newIORef visualisation
    viewParams <- newIORef (ViewParams 1 (-25, 35) (0, 0))

    reshapeCallback $= Just reshape
    depthFunc $= Just Less
    keyboardMouseCallback $= Just (keyboardMouse viewParams)
    idleCallback $= Just (idle viewParams)
    displayCallback $= display vis viewParams
    mainLoop
-}

readInt :: String -> Int
readInt = read
