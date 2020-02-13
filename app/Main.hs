{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Main where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef

import Parsers
import Rendering
import Display
import Bindings
import Types

import Data.Either (fromRight)
import Data.Void
import Data.Text hiding (map, foldr)
import Text.Megaparsec

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize

    let testGraph = Graph TwoD "TEST" [(File "/home/jim/college/fyp/data.txt")] renderSquares
    let testVis = Vis testGraph ([Types.Black, Types.Red] :: ColourScheme)

    dataFile <- readFile $ "/home/jim/college/fyp/data.txt"
    let res = runParser pCSV2 "" $ pack dataFile
    if (not (success res)) then do
        print res
        error "Error reading data from file."
    else do
        let rawData = XY $ unzip $ fromRight [(0.0::Float,0.0::Float)] res
        {-
            Put parsed data into a XY GraphData yoke
            and make a new Vis
        -}

        let visualisation = Vis (Graph TwoD "TEST" [(XY ([1], [1]))] renderSquares) ([Types.Black, Types.Red] :: ColourScheme)
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
success (Right _)   = True
success _           = False
