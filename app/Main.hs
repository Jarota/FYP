module Main where

import Graphics.UI.GLUT
import Data.IORef

import Display
import Bindings
import Types

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Hello World"


  let visualisation = Vis (Scatter2D "TEST" [(Raw ([1, 2, 3], [7, 8, 9]))]) ([Types.Black, Types.Red] :: ColourScheme)
  vis <- newIORef visualisation
  zoom <- newIORef 1

  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (keyboardMouse zoom)
  idleCallback $= Just (idle zoom)
  displayCallback $= display vis zoom
  mainLoop
