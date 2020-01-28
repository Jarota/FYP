module Vis (renderVis) where

import Graphics.UI.GLUT
import Control.Monad

import Types
import Graphs

renderVis :: Vis GLfloat GLfloat -> IO ()
renderVis (Vis graph (c:cs)) = do
        clearColor $= (convertColour c)
        clear [ColorBuffer, DepthBuffer]
        renderGraph graph cs
