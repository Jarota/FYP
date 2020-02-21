module Rendering where

import Graphics.UI.GLUT
import Types

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderLines :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderLines [] = return ()
renderLines ps = renderPrimitive Lines $ mapM_ vertex3f ps

renderLine :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderLine [] _     = return ()
renderLine ps _     = renderPrimitive LineStrip $ mapM_ vertex3f ps

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
renderBars [] _     = return ()
renderBars ps width = renderPrimitive Quads $ mapM_ vertex3f ps'
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

bg2D :: [(GLfloat, GLfloat, GLfloat)]
bg2D = [
        (-1, 1, 0), (-0.8, 1, 0), (-0.8, -0.8, 0), (-1, -0.8, 0),   -- Left
        (-1, -0.8, 0), (1, -0.8, 0), (1, -1, 0), (-1, -1, 0)        -- Bottom
    ]

bg3D :: [(GLfloat, GLfloat, GLfloat)]
bg3D = [
        (-1, 1, 0), (-0.8, 1, 0), (-0.8, -0.8, 0), (-1, -0.8, 0),   -- Left
        (-1, -0.8, 0), (1, -0.8, 0), (1, -1, 0), (-1, -1, 0)        -- Bottom
    ]

renderTitle :: String -> IO ()
renderTitle title = preservingMatrix $ do
    scale 0.001 0.001 (0.001::GLfloat)
    color $ convertColour Types.White
    width <- stringWidth Roman title
    let offset = (fromIntegral width)/2
    translate $ Vector3 (-offset) 800 (0::GLfloat)
    renderString Roman title

axisLabels2D :: [String] -> IO ()
axisLabels2D (y:(x:_)) = preservingMatrix $ do
    color $ convertColour Types.White
    scale 0.0005 0.0005 (0.0005::GLfloat)
    preservingMatrix $ do
        widthY <- stringWidth Roman y
        let offsetY = (fromIntegral widthY)/2
        rotate 90 $ Vector3 0 0 (1::GLfloat)
        translate $ Vector3 (-offsetY) (1800) (0::GLfloat)
        renderString Roman y
    preservingMatrix $ do
        widthX <- stringWidth Roman x
        let offsetX = (fromIntegral widthX)/2
        translate $ Vector3 (-offsetX) (-1800) (0::GLfloat)
        renderString Roman x


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
