module Axes where

import Graphics.UI.GLUT
import Rendering


axes2D :: IO ()
axes2D = do
    color $ Color4 0 0 0 (1::GLfloat)
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.7, -0.7, -1), (0.7, -0.7, -1),
        (-0.7, -0.7, -1), (-0.7, 0.7, -1) ]

axes3D :: IO ()
axes3D = do
    color $ Color4 0 0 0 (1::GLfloat)
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.7, -0.7, -0.7), (0.7, -0.7, -0.7),
        (-0.7, -0.7, -0.7), (-0.7, 0.7, -0.7),
        (-0.7, -0.7, -0.7), (-0.7, -0.7, 0.7) ]


axisLabels2D :: (String,String) -> IO ()
axisLabels2D (x,y) = preservingMatrix $ do
    color $ Color4 0 0 0 (1::GLfloat)
    scale 0.0005 0.0005 (0.0005::GLfloat)
    preservingMatrix $ do
        widthY <- stringWidth Roman y
        let offsetY = (fromIntegral widthY)/2
        rotate 90 $ Vector3 0 0 (1::GLfloat)
        translate $ Vector3 (-offsetY) (1800) (-2000::GLfloat)
        renderString Roman y
    preservingMatrix $ do
        widthX <- stringWidth Roman x
        let offsetX = (fromIntegral widthX)/2
        translate $ Vector3 (-offsetX) (-1800) (-2000::GLfloat)
        renderString Roman x

axisLabels3D :: (String,String,String) -> [GLfloat] -> IO ()
axisLabels3D (x,y,z) m = preservingMatrix $ do
    color $ Color4 0 0 0 (1::GLfloat)
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
