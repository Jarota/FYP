{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Main where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef

import Parsers
import Rendering
import Display
import Bindings
import Types

import Data.Either (rights)
import Data.Void
import Data.Text hiding (map, foldr)
import Text.Megaparsec

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize

    let testGraph = Graph TwoD renderLine "Yeehaw" [(File "/home/jim/college/fyp/data2.csv")]
    let inputVis = Vis testGraph ([Types.Orange, Types.Blue :: Colour])

    let paths = getVisPaths inputVis
    let ioFiles = map readFile paths
    dataFiles <- sequence ioFiles
    let dataFiles' = map pack dataFiles
    let graphData = toGraphData (visType inputVis) dataFiles'

    let visualisation = replaceVisPaths inputVis graphData
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


toGraphData :: GraphType -> [Text] -> [GraphData]
toGraphData TwoD dataFiles = graphData
    where
        parsedData = map (runParser pCSV2 "") dataFiles
        res = rights parsedData
        graphData = map XY $ map unzip res

toGraphData ThreeD dataFiles = graphData
    where
        parsedData = map (runParser pCSV3 "") dataFiles
        res = rights parsedData
        graphData = map XYZ $ map unzip3 res
