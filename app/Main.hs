{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.GLUT
import Data.IORef

import Parsers
import Display
import Bindings
import Types

import Data.Void
import Data.Text
import Text.Megaparsec

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize

    let testGraph = Graph Scatter2D "TEST" [(File "/home/jim/college/fyp/data.txt")]
    let testVis = Vis testGraph ([Types.Black, Types.Red] :: ColourScheme)

    dataFile <- readFile $ visDataFile testVis
    let res = runParser pCSV "" $ pack dataFile
    if (not (success res)) then do
        print res
        error "Error reading data from file."
    else do

        {-
            Put parsed data into a XY GraphData yoke
            and make a new Vis
        -}


        vis <- newIORef visualisation
        viewParams <- newIORef (ViewParams 1 (-25, 35) (0, 0))

        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
        _window <- createWindow "DataVis"

        reshapeCallback $= Just reshape
        depthFunc $= Just Less
        keyboardMouseCallback $= Just (keyboardMouse viewParams)
        idleCallback $= Just (idle viewParams)
        displayCallback $= display vis viewParams
        mainLoop

success :: Either l r -> Bool
success (Right _)	= True
success _			= False
