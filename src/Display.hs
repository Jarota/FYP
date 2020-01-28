module Display (display, idle) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Types
import Vis
import Cube

display :: IORef (Vis GLfloat GLfloat)-> IORef GLfloat -> DisplayCallback
display visRef zoomRef = do
    vis <- get visRef
    zoom <- get zoomRef

    loadIdentity
    preservingMatrix $ do
        scale zoom zoom (1::GLfloat)
        renderVis vis
    swapBuffers

idle :: IORef GLfloat -> IdleCallback
idle zoom = do
    postRedisplay Nothing
