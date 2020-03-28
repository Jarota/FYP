module Display (display, idle) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Graph
import Visualisation

display :: Graph a => IORef (Visualisation a) -> DisplayCallback
display visRef = do
    lineSmooth $= Enabled
    pointSmooth $= Enabled
    polygonSmooth $= Enabled
    clearColor $= Color4 0.6 0.6 0.6 1
    clear [ColorBuffer, DepthBuffer]

    vis <- get visRef
    loadIdentity
    renderVis vis

    flush
    swapBuffers

idle :: IdleCallback
idle = do
    postRedisplay Nothing
