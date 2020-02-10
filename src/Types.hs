module Types where

import Graphics.UI.GLUT

data GraphData a    = XY ([a], [a])
                    | XYZ ([a], [a], [a])
                    -- | File String

type AxisTitles = (String, String)
type GraphTitle = String

data Graph a    = Scatter2D GraphTitle [GraphData a]
                | Bar2D GraphTitle [GraphData a]
                | Scatter3D GraphTitle [GraphData a]

data Colour = Red | Green | Blue | White | Grey | Black | Orange

{- A list of colours, starting with background colour,
    followed by colours to use for each data set -}
type ColourScheme = [Colour]

-- Window size in pixels
-- type Size = (Int, Int)

data Vis a  = Vis (Graph a) ColourScheme -- Size

data ViewParams = ViewParams {
    zoom :: GLfloat,            -- scale factor
    rot :: (GLfloat, GLfloat),  -- angles to rotate by (y axis, x axis)
    pan :: (GLfloat, GLfloat)   -- position to translate vis by
}

convertColour :: Colour -> Color4 GLfloat
convertColour c = case c of
    Types.Red       -> Color4 1 0 0 1
    Types.Green     -> Color4 0 1 0 1
    Types.Blue      -> Color4 0 0 1 1
    Types.White     -> Color4 1 1 1 1
    Types.Grey      -> Color4 0.6 0.6 0.6 1
    Types.Black     -> Color4 0 0 0 1
    Types.Orange    -> Color4 1 0.5 0 1

--                    Points To Render                 Size
type RenderFunction = [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
