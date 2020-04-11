{-# LANGUAGE RecordWildCards #-}

module Legend where

import Graphics.UI.GLUT

import DataSet
import Rendering (renderSquares)

--              Data            Position of Legend
renderLegend :: [DataSet] -> (GLfloat, GLfloat) -> IO ()
renderLegend ds (x,y) = do
    loadIdentity
    translate $ Vector3 x y (-1)
    renderKeys ds

renderKeys :: [DataSet] -> IO ()
renderKeys []     = return ()
renderKeys (d:ds) = do
    renderKey d
    renderKeys ds

renderKey :: DataSet -> IO ()
renderKey File{..}    = return ()
renderKey Raw{..}     = do
    -- Translate down to appropriate position in list
    translate $ Vector3 0 (-0.1) (0::GLfloat)

    -- Render small square of correct color
    preservingMatrix $ do
        color dataColor
        renderSquares [(0,0,0)] 0.02

    -- Render the dataset's label slightly to the right
    preservingMatrix $ do
        color $ Color4 0 0 0 (1::GLfloat)
        translate $ Vector3 0.1 (-0.02) (0::GLfloat)
        scale 0.0005 0.0005 (0.0005::GLfloat)
        renderString Roman label
