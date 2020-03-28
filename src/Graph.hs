module Graph where

import Graphics.UI.GLUT (Color4, GLfloat)
import ViewParams


class Graph a where
    dimensions :: a -> Int
    dimensions _ = 2

    format :: a -> a
    format a = a
    
    render :: a -> ViewParams -> IO ()
    render _ _ = return ()
