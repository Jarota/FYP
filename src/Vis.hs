module Vis (renderVis) where

import Graphics.UI.GLUT
import Control.Monad

import Types
import Graphs

renderVis :: Vis -> ViewParams -> IO ()
renderVis (Vis graph cs) viewParams = do
        clearColor $= (convertColour $ head cs)
        clear [ColorBuffer, DepthBuffer]
        renderGraph graph cs viewParams
