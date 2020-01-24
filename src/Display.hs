module Display (display, idle) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import Types
import Vis

display :: IORef (Vis x y)-> IORef GLfloat -> DisplayCallback
display visRef zoomRef = do
    vis <- get visRef
    zoom <- get zoomRef

    loadIdentity
    scale zoom zoom (1::GLfloat)
    preservingMatrix $ do
        renderVis vis

        swapBuffers

idle :: IORef GLfloat -> IdleCallback
idle zoom = do
    postRedisplay Nothing
