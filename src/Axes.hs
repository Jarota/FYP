module Axes where

import Graphics.UI.GLUT
import Types
import Rendering


axes2D :: IO ()
axes2D = do
    color $ convertColour Types.White
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.8, -0.8, -0.8), (0.8, -0.8, -0.8),
        (-0.8, -0.8, -0.8), (-0.8, 0.8, -0.8) ]

axes3D :: IO ()
axes3D = do
    color $ convertColour Types.White
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.8, -0.8, -0.8), (0.8, -0.8, -0.8),
        (-0.8, -0.8, -0.8), (-0.8, 0.8, -0.8),
        (-0.8, -0.8, -0.8), (-0.8, -0.8, 0.8) ]


renderTicks2D :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks2D (stepX, offX) (stepY, offY)   | stepX <= 0.01 || stepY <= 0.01 = return ()
                                            | otherwise = do
                                                color $ convertColour Types.White
                                                print "Made it to Here!"
                                                renderLines xs
                                                renderLines ys
                                                where
                                                    xs = generateTicks pointToTickX offX stepX
                                                    ys = generateTicks pointToTickY offY stepY

renderTicks3D :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks3D (stepX, offX) (stepY, offY) (stepZ, offZ) | stepX <= 0.01 || stepY <= 0.01 || stepZ <= 0.01 = return ()
                                                        | otherwise = do
                                                            color $ convertColour Types.White
                                                            renderLines xs
                                                            renderLines ys
                                                            renderLines zs
                                                            where
                                                                xs = generateTicks pointToTickX offX stepX
                                                                ys = generateTicks pointToTickY offY stepY
                                                                zs = generateTicks pointToTickZ offZ stepZ


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
