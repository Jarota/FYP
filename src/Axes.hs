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
        translate $ Vector3 (-offsetY-1800) 0 (-2000::GLfloat)
        renderString Roman y
    preservingMatrix $ do
        widthX <- stringWidth Roman x
        let offsetX = (fromIntegral widthX)/2
        translate $ Vector3 (-offsetX) (-1800) (-2000::GLfloat)
        renderString Roman x

axisLabels3D :: (String,String,String) -> [IO ()] -> IO ()
axisLabels3D (x,y,z) rs = preservingMatrix $ do
    color $ Color4 0 0 0 (1::GLfloat)
    -- scale because text renders very big
    scale 0.0005 0.0005 (0.0005::GLfloat)
    
    preservingMatrix $ do
        translate $ Vector3 (-1800) 0 (-2000::GLfloat)
        preservingMatrix $ do
            sequence rs
            widthY <- stringWidth Roman y
            let offsetY = (fromIntegral widthY)/2
            translate $ Vector3 (-offsetY) 0 (0::GLfloat)
            renderString Roman y

    preservingMatrix $ do
        translate $ Vector3 0 (-1800) (-2000::GLfloat)
        preservingMatrix $ do
            sequence rs
            widthX <- stringWidth Roman x
            let offsetX = (fromIntegral widthX)/2
            translate $ Vector3 (-offsetX) 0 (0::GLfloat)
            renderString Roman x
    
    preservingMatrix $ do
        translate $ Vector3 (-1800) (-1800) (0::GLfloat)
        preservingMatrix $ do
            sequence rs
            widthZ <- stringWidth Roman z
            let offsetZ = (fromIntegral widthZ)/2
            translate $ Vector3 (-offsetZ) 0 (0::GLfloat)
            renderString Roman z
