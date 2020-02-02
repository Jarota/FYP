module Rendering where

import Graphics.UI.GLUT
import Types

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

renderLines :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderLines [] = return ()
renderLines xs = renderPrimitive Lines $ mapM_ vertex3f xs

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

renderTicks :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks (stepX, offX) (stepY, offY) | stepX <= 0.01 || stepY <= 0.01 = return ()
                                        | otherwise = do
                                            color $ convertColour Types.White
                                            renderLines xs
                                            renderLines ys
                                            where
                                                xs = generateTicks pointToTickX offX stepX
                                                ys = generateTicks pointToTickY offY stepY

generateTicks :: ( GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)] ) -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
generateTicks pointToTick offset step = concatMap (pointToTick offset) steps
    where
        maxSteps = 2/step
        steps = map (*step) [0..maxSteps]

pointToTickX :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickX offset x   | x-offset > 0.8   = []
                        | otherwise = [(x-offset, -0.8, 0), (x-offset, -0.78, 0)]

pointToTickY :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickY offset y   | y-offset > 0.8   = []
                        | otherwise = [(-0.8, y-offset, 0), (-0.78, y-offset, 0)]


redoBackground :: IO ()
redoBackground = renderPrimitive Quads $ mapM_ vertex3f bgPoints

bgPoints :: [(GLfloat, GLfloat, GLfloat)]
bgPoints = [
        (-1, 1, 0), (-0.8, 1, 0), (-0.8, -0.8, 0), (-1, -0.8, 0),   -- Left
        (-1, -0.8, 0), (1, -0.8, 0), (1, -1, 0), (-1, -1, 0)        -- Bottom
    ]

axes2D :: IO ()
axes2D = do
    color $ convertColour Types.White
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.8, 0.8, 0), (-0.8, -0.8, 0),
        (-0.8, -0.8, 0), (0.8, -0.8, 0) ]

renderTitle :: String -> IO ()
renderTitle title = preservingMatrix $ do
    scale 0.001 0.001 (0.001::GLfloat)
    color $ convertColour Types.White
    translate $ Vector3 0 100 (0::GLfloat)
    renderString Roman title
