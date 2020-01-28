module Rendering where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderSquares :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSquares [] = return ()
renderSquares ps = renderPrimitive Quads $ mapM_ vertex3f ps

pointToSquare :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToSquare l (x, y, z) = [
        (x-l, y+l, z),
        (x+l, y+l, z),
        (x+l, y-l, z),
        (x-l, y-l, z)
    ]
