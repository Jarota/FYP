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


  let visualisation = Vis (Scatter2D "TEST" [(Raw ([1, 3, 4, 5, 6, 7, 8, 9], [2, 1, 7, 3, 4, 8, 6, 9]))]) ([Types.Black, Types.Red] :: ColourScheme)
  vis <- newIORef visualisation
  viewParams <- newIORef (ViewParams 1 (0, 0) (0, 0))

  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (keyboardMouse viewParams)
  idleCallback $= Just (idle viewParams)
  displayCallback $= display vis viewParams
  mainLoop
