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
    (MouseButton LeftButton)    -> vp $~! rotating
    (MouseButton WheelDown)     -> vp $~! zoomOut
    (MouseButton WheelUp)       -> vp $~! zoomIn
    (Char 'x')                  -> vp $~! viewAlongX
    (Char 'y')                  -> vp $~! viewAlongY
    (Char 'z')                  -> vp $~! viewAlongZ
    (Char 'q')                  -> leaveMainLoop

    _ -> return ()

threeDControls vp key Up _ _ = case key of
    (MouseButton LeftButton)    -> vp $~! notRotating
    _ -> return ()


{- Zooming and Panning Controls and Quit -}
twoDControls :: IORef ViewParams -> KeyboardMouseCallback
twoDControls vp key Down _ _ = case key of
    (MouseButton LeftButton)    -> vp $~! panning
    (MouseButton WheelDown)     -> vp $~! zoomOut
    (MouseButton WheelUp)       -> vp $~! zoomIn
    (Char 'q')                  -> leaveMainLoop
    _ -> return ()

twoDControls vp key Up _ _ = case key of
    (MouseButton LeftButton)    -> vp $~! notPanning
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
rotating (ViewParams ts z _ p) = ViewParams ts z True p

notRotating :: ViewParams -> ViewParams
notRotating (ViewParams ts z _ p) = ViewParams ts z False p

panning :: ViewParams -> ViewParams
panning (ViewParams ts z r _) = ViewParams ts z r True

notPanning :: ViewParams -> ViewParams
notPanning (ViewParams ts z r _) = ViewParams ts z r False

zoomIn :: ViewParams -> ViewParams
zoomIn (ViewParams ts z r p) = ViewParams ts (z+0.05) r p

zoomOut :: ViewParams -> ViewParams
zoomOut (ViewParams ts z r p)
    | z' <= 0   = ViewParams ts z r p
    | otherwise = ViewParams ts z' r p
    where
        z' = (z-0.05)

panView :: ViewParams -> Position -> Position -> Size -> ViewParams
panView (ViewParams ts z r p) pos1 pos2 size = ViewParams (t:ts) z r p
    where
        (x1, y1) = screenToWorld pos1 size
        (x2, y2) = screenToWorld pos2 size
        (x, y)   = (x2-x1, y2-y1)
        t        = translate $ Vector3 x (-y) (0::GLdouble)

rotView :: ViewParams -> Position -> Position -> Size -> ViewParams
rotView (ViewParams ts z r p) pos1 pos2 size
    | pos1 == pos2  = ViewParams ts z r p
    | otherwise     = ViewParams (t++ts) z r p
    where
        (x1, y1)    = screenToWorld pos1 size
        (x2, y2)    = screenToWorld pos2 size
        x           = x2-x1
        y           = y2-y1
        h           = sqrt $ (x^2) + (y^2)
        zAngle      = angleToPrimaryAxis x y h
        xAngle      = h * (-90)
        t           = arbitraryRotation zAngle xAngle

arbitraryRotation :: GLdouble -> GLdouble -> [IO ()]
arbitraryRotation zAngle xAngle = [
        (rotate (-zAngle) $ Vector3 0 0 1),
        (rotate xAngle $ Vector3 1 0 0),
        (rotate zAngle $ Vector3 0 0 1)
    ]

angleToPrimaryAxis :: GLdouble -> GLdouble -> GLdouble -> GLdouble
angleToPrimaryAxis x y h
    | x >= 0    = 270 + angle
    | x < 0     = 90 - angle
    where
        angle = toDegrees $ asin ( y / h )

toDegrees x = x * (180/pi)

screenToWorld :: Position -> Size -> (GLdouble, GLdouble)
screenToWorld (Position x y) (Size w h) = (x'-1, y'-1)
    where
        x' = ((fromIntegral x) / (fromIntegral w))*2
        y' = ((fromIntegral y) / (fromIntegral h))*2

viewAlongX :: ViewParams -> ViewParams
viewAlongX (ViewParams _ z r p) = ViewParams [(rotate (90::GLfloat) $ Vector3 0 1 0)] z r p

viewAlongY :: ViewParams -> ViewParams
viewAlongY (ViewParams _ z r p) = ViewParams [(rotate (90::GLfloat) $ Vector3 0 0 1), (rotate (90::GLfloat) $ Vector3 1 0 0)] z r p

viewAlongZ :: ViewParams -> ViewParams
viewAlongZ (ViewParams _ z r p) = ViewParams [] z r p