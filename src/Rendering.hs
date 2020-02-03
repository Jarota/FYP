module Rendering where

import Graphics.UI.GLUT
import Types

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderLines :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderLines [] = return ()
renderLines xs = renderPrimitive Lines $ mapM_ vertex3f xs

renderSquares :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderSquares [] _      = return ()
renderSquares ps width  = renderPrimitive Quads $ mapM_ vertex3f ps'
    where
        ps' = concatMap (pointToSquare width) ps

pointToSquare :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToSquare l (x, y, z) = [
        (x-l, y+l, z),
        (x+l, y+l, z),
        (x+l, y-l, z),
        (x-l, y-l, z)
    ]

renderCubes :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderCubes [] _        = return ()
renderCubes ps width    = do
    renderPrimitive Quads $ mapM_ vertex3f ps'
    color $ convertColour Types.White
    renderPrimitive Lines $ mapM_ vertex3f ps''
    where
        ps' = concatMap (pointToCube width) ps
        ps'' = concatMap (pointToCubeFrame width) ps

renderBars :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderBars [] _      = return ()
renderBars ps width  = renderPrimitive Quads $ mapM_ vertex3f ps'
    where
        ps' = concatMap (pointToBar width) ps

pointToBar :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToBar l (x, y, z) = [
        (x-l, y+l, z),
        (x+l, y+l, z),
        (x+l, -0.8, z),
        (x-l, -0.8, z)
    ]

redoBackground :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
redoBackground ps = renderPrimitive Quads $ mapM_ vertex3f ps

bgPoints2D :: [(GLfloat, GLfloat, GLfloat)]
bgPoints2D = [
        (-1, 1, 0.8), (-0.8, 1, 0.8), (-0.8, -0.8, 0.8), (-1, -0.8, 0.8),   -- Left
        (-1, -0.8, 0.8), (1, -0.8, 0.8), (1, -1, 0.8), (-1, -1, 0.8)        -- Bottom
    ]

bgPoints3D :: [(GLfloat, GLfloat, GLfloat)]
bgPoints3D = [
        (-1, 1, 0), (-0.8, 1, 0), (-0.8, -0.8, 0), (-1, -0.8, 0),   -- Left
        (-1, -0.8, 0), (1, -0.8, 0), (1, -1, 0), (-1, -1, 0)        -- Bottom
    ]

renderTitle :: String -> IO ()
renderTitle title = preservingMatrix $ do
    scale 0.001 0.001 (0.001::GLfloat)
    color $ convertColour Types.White
    translate $ Vector3 0 100 (0::GLfloat)
    renderString Roman title

pointToCube :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToCube l (x, y, z) = [
        -- front face
        (x-l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y-l, z+l),
        (x-l, y-l, z+l),
        -- back face
        (x-l, y+l, z-l),
        (x+l, y+l, z-l),
        (x+l, y-l, z-l),
        (x-l, y-l, z-l),
        -- left face
        (x-l, y+l, z-l),
        (x-l, y+l, z+l),
        (x-l, y-l, z+l),
        (x-l, y-l, z-l),
        -- right face
        (x+l, y+l, z-l),
        (x+l, y+l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z-l),
        -- top face
        (x-l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y+l, z-l),
        (x-l, y+l, z-l),
        -- bottom face
        (x-l, y-l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z-l),
        (x-l, y-l, z-l)
    ]

pointToCubeFrame :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToCubeFrame l (x, y, z) = [
        -- front face
        (x-l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z+l),
        (x-l, y-l, z+l),
        (x-l, y-l, z+l),
        (x-l, y+l, z+l),
        -- back face
        (x-l, y+l, z-l),
        (x+l, y+l, z-l),
        (x+l, y+l, z-l),
        (x+l, y-l, z-l),
        (x+l, y-l, z-l),
        (x-l, y-l, z-l),
        (x-l, y-l, z-l),
        (x-l, y+l, z-l),
        -- left face
        (x-l, y+l, z-l),
        (x-l, y+l, z+l),
        (x-l, y+l, z+l),
        (x-l, y-l, z+l),
        (x-l, y-l, z+l),
        (x-l, y-l, z-l),
        (x-l, y-l, z-l),
        (x-l, y+l, z-l),
        -- right face
        (x+l, y+l, z-l),
        (x+l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z-l),
        (x+l, y-l, z-l),
        (x+l, y+l, z-l),
        -- top face
        (x-l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y+l, z+l),
        (x+l, y+l, z-l),
        (x+l, y+l, z-l),
        (x-l, y+l, z-l),
        (x-l, y+l, z-l),
        (x-l, y+l, z+l),
        -- bottom face
        (x-l, y-l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z+l),
        (x+l, y-l, z-l),
        (x+l, y-l, z-l),
        (x-l, y-l, z-l),
        (x-l, y-l, z-l),
        (x-l, y-l, z+l)
    ]
