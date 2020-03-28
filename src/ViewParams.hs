module ViewParams where

import Graphics.UI.GLUT

data ViewParams = ViewParams {
    transformations :: [IO ()],
    zoom :: GLfloat,
    rot :: Bool,
    pan :: Bool
}