module Rendering where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderTitle :: String -> IO ()
renderTitle title = preservingMatrix $ do
    scale 0.001 0.001 (0.001::GLfloat)
    color $ Color4 0 0 0 (1::GLfloat)
    width <- stringWidth Roman title
    let offset = (fromIntegral width)/2
    translate $ Vector3 (-offset) 850 (-1000::GLfloat)
    renderString Roman title

renderFrame :: IO ()
renderFrame = do
    color $ Color4 0.85 0.85 0.85 (1::GLfloat)
    renderPrimitive Quads $ mapM_ vertex3f frame

frame :: [(GLfloat, GLfloat, GLfloat)]
frame = [
        (-1, 1, -0.9), (-0.7, 1, -0.9), (-0.7, -1, -0.9), (-1, -1, -0.9), -- Left
        (1, 1, -0.9), (0.7, 1, -0.9), (0.7, -1, -0.9), (1, -1, -0.9), -- Right
        (-0.7, -0.7, -0.9), (0.7, -0.7, -0.9), (0.7, -1, -0.9), (-0.7, -1, -0.9), -- Bottom
        (-0.7, 0.7, -0.9), (0.7, 0.7, -0.9), (0.7, 1, -0.9), (-0.7, 1, -0.9) -- Top
    ]

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

renderBars :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderBars [] _     = return ()
renderBars ps width = renderPrimitive Quads $ mapM_ vertex3f ps'
    where
        ps' = concatMap (pointToBar width) ps

pointToBar :: GLfloat -> (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat)]
pointToBar l (x, y, z) = [
        (x-l, y+l, z),
        (x+l, y+l, z),
        (x+l, -0.7, z),
        (x-l, -0.7, z)
    ]

renderCubes :: [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderCubes [] _        = return ()
renderCubes points width    = do
    renderPrimitive Quads $ mapM_ vertex3f ps
    color $ Color4 1 1 1 (1::GLfloat)
    renderPrimitive Lines $ mapM_ vertex3f ps'
    where
        ps = concatMap (pointToCube width) points
        -- extra width to avoid 'z-fighting'
        ps' = concatMap (pointToCubeFrame (width+0.003)) points

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
