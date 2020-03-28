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
renderTicks2D (stepX, offX) (stepY, offY) = do
        color $ convertColour Types.White
        renderLines xs
        renderLines ys
        where
            xs = generateTicks pointToTickX offX $ skipStepsIfSmall stepX 1
            ys = generateTicks pointToTickY offY $ skipStepsIfSmall stepY 1

renderTicks3D :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
renderTicks3D (stepX, offX) (stepY, offY) (stepZ, offZ) = do
        color $ convertColour Types.White
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

axisLabels3D :: AxisLabels -> [GLfloat] -> IO ()
axisLabels3D (x:(y:(z:_))) m = preservingMatrix $ do
    color $ convertColour Types.White
    -- scale because text renders very big
    scale 0.0005 0.0005 (0.0005::GLfloat)
    
    -- transpose will undo rotations of modelview s.t. labels still face the screen
    let m' = reverseRotations m
    mT <- newMatrix ColumnMajor m' :: IO (GLmatrix GLfloat)
    preservingMatrix $ do
        widthY <- stringWidth Roman y
        let offsetY = (fromIntegral widthY)/2
        translate $ Vector3 (-1850) 0 (-1850::GLfloat)
        preservingMatrix $ do
            -- let depthVar = stackDepth $ Just $ Modelview 0
            -- depth <- get depthVar
            -- let modelView = matrix $ Just $ Modelview depth :: StateVar (GLmatrix GLfloat)
            withMatrix mT ( \major ptr -> do
                    rotate 90 $ Vector3 0 0 (1::GLfloat)
                    translate $ Vector3 (-offsetY) 0 (0::GLfloat)
                    renderString Roman y
                )
    preservingMatrix $ do
        widthX <- stringWidth Roman x
        let offsetX = (fromIntegral widthX)/2
        translate $ Vector3 (-offsetX) (-1850) (-1850::GLfloat)
        -- modelView $= mT
        renderString Roman x
    preservingMatrix $ do
        widthZ <- stringWidth Roman z
        let offsetZ = (fromIntegral widthZ)/2
        translate $ Vector3 (-offsetZ-1850) (-1850) (0::GLfloat)
        -- modelView $= mT
        renderString Roman z

reverseRotations :: [GLfloat] -> [GLfloat]
-- reverseRotations m = [
--         m!!0, m!!4, m!!8, m!!12,
--         m!!1, m!!5, m!!9, m!!13,
--         m!!2, m!!6, m!!10, m!!14,
--         m!!3, m!!7, m!!11, m!!15
--     ]
reverseRotations _ = [
        100,0,0,0,
        0,100,0,0,
        0,0,100,0,
        0,0,0,1
    ]
