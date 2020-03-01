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

axisLabels2D :: AxisLabels -> IO ()
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

axisLabels3D :: AxisLabels -> [IO ()] -> IO ()
axisLabels3D (z:(y:(x:_))) rs = preservingMatrix $ do
    color $ convertColour Types.White
    scale 0.0005 0.0005 (0.0005::GLfloat)
    preservingMatrix $ do
        widthY <- stringWidth Roman y
        let offsetY = (fromIntegral widthY)/2
        translate $ Vector3 (-1850) 0 (-1850::GLfloat)
        preservingMatrix $ do
            -- TODO undo rs
            rotate 90 $ Vector3 0 0 (1::GLfloat)
            translate $ Vector3 (-offsetY) 0 (0::GLfloat)
            renderString Roman y
    preservingMatrix $ do
        widthX <- stringWidth Roman x
        let offsetX = (fromIntegral widthX)/2
        translate $ Vector3 (-offsetX) (-1850) (-1850::GLfloat)
        -- TODO undo rs
        renderString Roman x
    preservingMatrix $ do
        widthZ <- stringWidth Roman z
        let offsetZ = (fromIntegral widthZ)/2
        translate $ Vector3 (-offsetZ-1850) (-1850) (0::GLfloat)
        -- TODO undo rs
        renderString Roman z
