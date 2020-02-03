module Display (display, idle) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Types
import Vis

display :: IORef (Vis GLfloat)-> IORef ViewParams -> DisplayCallback
display visRef vpRef = do
    vis <- get visRef
    viewParams <- get vpRef

    loadIdentity
    renderVis vis viewParams
    swapBuffers

idle :: IORef ViewParams -> IdleCallback
idle viewParams = do
    postRedisplay Nothing
