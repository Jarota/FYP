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

{- OLD -}
import Parsers
-- import Types

{- NEW -}
import DataSet
import GraphData
import ViewParams
import Visualisation
import Graph
import Scatter2D -- for demo graph
import Rendering
import Bindings
import Display

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    -- vis <- parseInput _args -- returns demo vis if _args is empty
    -- if isLeft vis
    --     then do
    --         let errors = fromLeft defaultErrorBundle vis
    --         putStrLn (errorBundlePretty errors)
    --     else do
    --         let inputVis = fromRight demoVis vis
            
            {- 
                TODO redo the parsing of csv files 
                OR parse them as they come in Parser.hs?
            -}
            
    let visualisation = demoVis
    visRef <- newIORef visualisation
    viewParamsRef <- newIORef initViewParams

    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize $= Size 800 800
    _window <- createWindow "DataVis"
    reshapeCallback $= Just reshape
    depthFunc $= Just Less

    pos <- newIORef initPos
    keyboardMouseCallback $= Just ( keyboardMouse (dimensions demoGraph) viewParamsRef )
    motionCallback $= Just ( mouseMotion viewParamsRef pos )
    passiveMotionCallback $= Just ( passiveMouseMotion pos )

    idleCallback $= Just idle
    displayCallback $= display visRef
    mainLoop

-- parseInput :: [String] -> Either (ParseErrorBundle Text Void) Vis
-- parseInput []   = return $ Right demoVis
-- parseInput args = do
--     input <- readFile $ head args
--     return $ runParser pVis "" $ pack input

initPos :: Position
initPos = Position (-1) (-1)

demoVis :: Visualisation Scatter2D
demoVis = Vis "TEST" demoGraph initViewParams

demoGraph :: Scatter2D
demoGraph = Scatter2D ("BADA","BING") [( Raw (Color4 0 0 0 (1::GLfloat)) "Label" [demoData, demoData])]

demoData :: GraphData
demoData = toGraphData [1,2,3,4,5::GLfloat]

initViewParams :: ViewParams
initViewParams = ViewParams [] 1 False False

-- defaultErrorBundle :: ParseErrorBundle e s
defaultErrorBundle = ParseErrorBundle ((TrivialError 0 Nothing S.empty)NE.:|[]) (PosState "" 0 (SourcePos "" (mkPos 0) (mkPos 0)) (mkPos 0) "")
