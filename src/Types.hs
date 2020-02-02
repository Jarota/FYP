module Types (
    GraphData (..),
    AxisTitles,
    GraphTitle,
    Graph (..),
    Colour (..),
    ColourScheme (..),
    Vis (..),
    ViewParams (..),
    convertColour
) where

import Graphics.UI.GLUT

data GraphData x y  = File String
                    | Raw ([x], [y]) --- maybe should be Raw [(x, y)]

type AxisTitles = (String, String)
type GraphTitle = String

data Graph x y = Scatter2D GraphTitle [GraphData x y]

data Colour = Red | Green | Blue | White | Grey | Black

{- A list of colours, starting with background colour,
    followed by colours to use for each data set -}
type ColourScheme = [Colour]

-- Window size in pixels
-- type Size = (Int, Int)

data Vis x y = Vis (Graph x y) ColourScheme -- Size

data ViewParams = ViewParams {
    zoom :: GLfloat,            -- scale factor
    pan :: (GLfloat, GLfloat),  -- position to translate vis by
    rot :: (GLfloat, GLfloat)   -- angles to rotate by (y axis, x axis)
}

convertColour :: Colour -> Color4 GLfloat
convertColour c = case c of
    Types.Red     -> Color4 1 0 0 1
    Types.Green   -> Color4 0 1 0 1
    Types.Blue    -> Color4 0 0 1 1
    Types.White   -> Color4 1 1 1 1
    Types.Grey    -> Color4 0.6 0.6 0.6 1
    Types.Black   -> Color4 0 0 0 1
