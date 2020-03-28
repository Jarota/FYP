module AxisTicks where

import Graphics.UI.GLUT
import Rendering

renderTicks2D :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks2D (stepX, offX) (stepY, offY) = do
        color $ Color4 1 1 1 (1::GLfloat)
        renderLines xs
        renderLines ys
        where
            xs = generateTicks pointToTickX offX $ skipStepsIfSmall stepX 1
            ys = generateTicks pointToTickY offY $ skipStepsIfSmall stepY 1

renderTicks3D :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks3D (stepX, offX) (stepY, offY) (stepZ, offZ) = do
        color $ Color4 1 1 1 (1::GLfloat)
        renderLines xs
        renderLines ys
        renderLines zs
        where
            xs = generateTicks pointToTickX offX $ skipStepsIfSmall stepX 1
            ys = generateTicks pointToTickY offY $ skipStepsIfSmall stepY 1
            zs = generateTicks pointToTickZ offZ $ skipStepsIfSmall stepZ 1

skipStepsIfSmall :: GLfloat -> GLfloat -> GLfloat
skipStepsIfSmall 0 _ = 1.6
skipStepsIfSmall x n
    | x*n >= 0.05   = x*n
    | otherwise     = skipStepsIfSmall x (n+1)

generateTicks :: ( GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)] ) -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
generateTicks pointToTick offset step = concatMap (pointToTick offset) steps
    where
        maxSteps = 10/step
        steps = map (*step) [0..maxSteps]

pointToTickX :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickX offset x   | x-offset > 0.8 || x-offset < -0.8 = []
                        | otherwise = [
                            (x-offset, -0.8, -0.8), (x-offset, -0.78, -0.8),
                            (x-offset, -0.8, -0.8), (x-offset, -0.8, -0.78)
                            ]

pointToTickY :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickY offset y   | y-offset > 0.8 || y-offset < -0.8 = []
                        | otherwise = [
                            (-0.8, y-offset, -0.8), (-0.78, y-offset, -0.8),
                            (-0.8, y-offset, -0.8), (-0.8, y-offset, -0.78)
                            ]

pointToTickZ :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickZ offset z   | z-offset > 0.8 || z-offset < -0.8 = []
                        | otherwise = [
                            (-0.8, -0.8, z-offset), (-0.78, -0.8, z-offset),
                            (-0.8, -0.8, z-offset), (-0.8, -0.78, z-offset)
                            ]