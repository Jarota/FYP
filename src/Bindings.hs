module Bindings (reshape, keyboardMouse, mouseMotion, passiveMouseMotion) where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef
import Display
import Types

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: GraphType -> IORef ViewParams -> KeyboardMouseCallback
keyboardMouse TwoD      = twoDControls
keyboardMouse ThreeD    = threeDControls


threeDControls :: IORef ViewParams -> KeyboardMouseCallback
threeDControls vp key Down _ _ = case key of
    (MouseButton RightButton)   -> vp $~! panning
    (MouseButton LeftButton)    -> vp $~! rotating
    (MouseButton WheelDown)     -> vp $~! zoomOut
    (MouseButton WheelUp)       -> vp $~! zoomIn
    (Char 'q')                  -> leaveMainLoop

    _ -> return ()

threeDControls vp key Up _ _ = case key of
    (MouseButton RightButton)   -> vp $~! notPanning
    (MouseButton LeftButton)    -> vp $~! notRotating
    _ -> return ()


{- Zooming and Panning Controls and Quit -}
twoDControls :: IORef ViewParams -> KeyboardMouseCallback
twoDControls vp key Down _ _ = case key of
    (MouseButton RightButton)    -> vp $~! panning
    (MouseButton WheelDown)     -> vp $~! zoomOut
    (MouseButton WheelUp)       -> vp $~! zoomIn
    (Char 'q')                  -> leaveMainLoop
    _ -> return ()

twoDControls vp key Up _ _ = case key of
    (MouseButton RightButton)    -> vp $~! notPanning
    _ -> return ()


{- PANNING and ROTATING -}
mouseMotion :: IORef ViewParams -> IORef Position -> MotionCallback
mouseMotion vp lastPos curPos = do
    vp' <- get vp
    if (pan vp') then do
        lPos <- get lastPos
        (_, size) <- get viewport
        let vp'' = panView vp' lPos curPos size
        writeIORef vp vp''
        writeIORef lastPos curPos
    else if (rot vp') then do
        lPos <- get lastPos
        (_, size) <- get viewport
        let vp'' = rotView vp' lPos curPos size
        writeIORef vp vp''
        writeIORef lastPos curPos
    else return ()

{- Update Mouse Position -}
passiveMouseMotion :: IORef Position -> MotionCallback
passiveMouseMotion lastPos curPos = writeIORef lastPos curPos


{- Helper Functions -}
rotating :: ViewParams -> ViewParams
rotating (ViewParams rs ts _ p) = ViewParams rs ts True p

notRotating :: ViewParams -> ViewParams
notRotating (ViewParams rs ts _ p) = ViewParams rs ts False p

panning :: ViewParams -> ViewParams
panning (ViewParams rs ts r _) = ViewParams rs ts r True

notPanning :: ViewParams -> ViewParams
notPanning (ViewParams rs ts r _) = ViewParams rs ts r False

zoomIn :: ViewParams -> ViewParams
zoomIn (ViewParams rs ts r p) = ViewParams rs (t:ts) r p
    where
        t = scale 1.1 1.1 (1.1 :: GLfloat)

zoomOut :: ViewParams -> ViewParams
zoomOut (ViewParams rs ts r p) = ViewParams rs (t:ts) r p
    where
        t = scale 0.9 0.9 (0.9 :: GLfloat)

panView :: ViewParams -> Position -> Position -> Size -> ViewParams
panView (ViewParams rs ts r p) pos1 pos2 size = ViewParams rs (t:ts) r p
    where
        (x1, y1) = screenToWorld pos1 size
        (x2, y2) = screenToWorld pos2 size
        (x, y)   = (x2-x1, y2-y1)
        t        = translate $ Vector3 x (-y) (0::GLdouble)

rotView :: ViewParams -> Position -> Position -> Size -> ViewParams
rotView (ViewParams rs ts r p) pos1 pos2 size
    | pos1 == pos2  = ViewParams rs ts r p
    | otherwise     = ViewParams (t++rs) (t++ts) r p
    where
        (x1, y1)    = screenToWorld pos1 size
        (x2, y2)    = screenToWorld pos2 size
        x           = x2-x1
        y           = y2-y1
        h           = sqrt $ (x^2) + (y^2)
        zAngle      = angleToPrimaryAxis x y h
        xAngle      = h * 90
        t           = arbitraryRotation zAngle xAngle

arbitraryRotation :: GLdouble -> GLdouble -> [IO ()]
arbitraryRotation zAngle xAngle = [
        (rotate (-zAngle) $ Vector3 0 0 1),
        (rotate xAngle $ Vector3 1 0 0),
        (rotate zAngle $ Vector3 0 0 1)
    ]

angleToPrimaryAxis :: GLdouble -> GLdouble -> GLdouble -> GLdouble
angleToPrimaryAxis 0 y _ | y < 0     = 0
                         | otherwise = 180
angleToPrimaryAxis x 0 _ | x < 0     = 270
                         | otherwise = 90
angleToPrimaryAxis x y h
    | x > 0 = 90 + angle
    | x < 0 = 270 - angle
    where
        angle = asin ( y / h )

screenToWorld :: Position -> Size -> (GLdouble, GLdouble)
screenToWorld (Position x y) (Size w h) = (x'-1, y'-1)
    where
        x' = ((fromIntegral x) / (fromIntegral w))*2
        y' = ((fromIntegral y) / (fromIntegral h))*2
