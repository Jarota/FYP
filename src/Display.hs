module Display (display, idle) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Types
import Vis

display :: IORef Vis-> IORef ViewParams -> DisplayCallback
display visRef vpRef = do
    lineSmooth $= Enabled
    pointSmooth $= Enabled
    polygonSmooth $= Enabled

    clear [ColorBuffer, DepthBuffer]
    
    vis <- get visRef
    viewParams <- get vpRef

    loadIdentity
    renderVis vis viewParams

    flush
    swapBuffers

idle :: IORef ViewParams -> IdleCallback
idle viewParams = do
    postRedisplay Nothing
