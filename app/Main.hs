{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Either (isLeft, fromLeft, rights, fromRight)
import Data.Void
import Data.Text hiding (map, head)
import Text.Megaparsec

import Parsers
import Visualisation
import ViewParams
import Graph
import DataSet
import Bindings
import Display

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    
    -- Parse the DSL program
    
    vis <- parseInput _args -- returns demo vis if _args is empty
    if isLeft vis
        then do
            let errors = fromLeft defaultErrorBundle vis
            putStrLn (errorBundlePretty errors)
        else do
            let inputVis = fromRight demoVis vis
            {- 
                TODO redo the parsing of csv files 
                OR parse them as they come in Parser.hs?
            -}
            let visualisation = formatVis inputVis

            writeFile "log.txt" $ show visualisation

            visRef <- newIORef visualisation

            -- Initialise OpenGL and assign functions to StateVars
            initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
            initialWindowSize $= Size 800 800
            _window <- createWindow "DataVis"
            reshapeCallback $= Just reshape
            depthFunc $= Just Less

            pos <- newIORef initPos
            keyboardMouseCallback $= Just ( keyboardMouse (dimensions $ graph visualisation) visRef )
            motionCallback $= Just ( mouseMotion visRef pos )
            passiveMotionCallback $= Just ( passiveMouseMotion pos )
            idleCallback $= Just idle
            displayCallback $= display visRef

            -- Enter the GLUT main loop
            mainLoop



{- Helper Functions -}

parseInput :: [String] -> IO ( Either (ParseErrorBundle Text Void) Visualisation )
parseInput []   = return $ Right demoVis
parseInput args = do
    input <- readFile $ head args
    return $ runParser pVisualisation "" $ pack input

initPos :: Position
initPos = Position (-1) (-1)

demoVis :: Visualisation
demoVis = Vis "TEST" demoGraph initViewParams

demoGraph :: Graph -- Axis Labels, Axis Tick Parameters, Data
demoGraph = Scatter3D ("BADA","BING","BOOM") ((0,0),(0,0),(0,0)) [( Raw "Label" (Color4 0.5 0 0.5 (1::GLfloat)) [demoData, demoData, demoData])]

demoData :: GraphData
demoData = toGraphData (Prelude.take 25 [1..]::[GLfloat])

-- defaultErrorBundle :: ParseErrorBundle e s
defaultErrorBundle = ParseErrorBundle ((TrivialError 0 Nothing S.empty)NE.:|[]) (PosState "" 0 (SourcePos "" (mkPos 0) (mkPos 0)) (mkPos 0) "")
