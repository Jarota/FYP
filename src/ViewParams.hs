module ViewParams where

import Graphics.UI.GLUT

data ViewParams = ViewParams {
    transformations :: [IO ()],
    zoom :: GLfloat,
    rot :: Bool,
    pan :: Bool
}

instance Show ViewParams where
    show (ViewParams _ _ _ _) = "ViewParams"

initViewParams :: ViewParams
initViewParams = ViewParams [] 1 False False