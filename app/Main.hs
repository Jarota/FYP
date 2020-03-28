{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef

import Parsers
import Rendering
import Display
import Bindings
import Vis
import Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Either (isLeft, fromLeft, rights, fromRight)
import Data.Void
import Data.Text hiding (map, head)
import Text.Megaparsec

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    vis <- parseInput _args
    if isLeft vis
        then do
            let errors = fromLeft defaultErrorBundle vis
            putStrLn (errorBundlePretty errors)
        else do
            let inputVis = fromRight demoVis vis
            let paths = getVisPaths inputVis
            let ioFiles = map readFile paths
            dataFiles <- sequence ioFiles
            let dataFiles' = map pack dataFiles
            let graphData = toGraphData (visType inputVis) dataFiles'

            let visualisation = fitVisData $ replaceVisPaths inputVis graphData
            vis <- newIORef visualisation
            viewParams <- newIORef (ViewParams [] 1.0 False False)

            initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
            initialWindowSize $= Size 800 800
            _window <- createWindow "DataVis"
            reshapeCallback $= Just reshape
            depthFunc $= Just Less

            p <- newIORef initPos
            keyboardMouseCallback $= Just ( keyboardMouse (visType visualisation) viewParams)
            motionCallback $= Just ( mouseMotion viewParams p)
            passiveMotionCallback $= Just ( passiveMouseMotion p)

            idleCallback $= Just (idle viewParams)
            displayCallback $= display vis viewParams
            mainLoop

-- parseInput :: [String] -> Either (ParseErrorBundle Text Void) Vis
parseInput []   = return $ Right demoVis
parseInput args = do
    input <- readFile $ head args
    return $ runParser pVis "" $ pack input


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

initPos :: Position
initPos = Position (-1) (-1)

demoVis :: Vis
demoVis = Vis demoGraph3D ([Types.Grey, Types.Orange :: Colour])

demoGraph2D :: Graph
demoGraph2D = Graph TwoD renderSquares "Demo" ["Population", "Time"] [File "/home/jim/college/fyp/data2.csv"]

demoGraph3D :: Graph
demoGraph3D = Graph ThreeD renderCubes "Demo" ["Population", "Time", "GDP"] [File "/home/jim/college/fyp/data31.csv"]

-- defaultErrorBundle :: ParseErrorBundle e s
defaultErrorBundle = ParseErrorBundle ((TrivialError 0 Nothing S.empty)NE.:|[]) (PosState "" 0 (SourcePos "" (mkPos 0) (mkPos 0)) (mkPos 0) "")
