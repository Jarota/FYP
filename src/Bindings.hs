module Bindings (reshape, keyboardMouse, mouseMotion, passiveMouseMotion) where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Data.IORef
import Display
import Visualisation
import ViewParams
import Graph

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: Int -> IORef Visualisation-> KeyboardMouseCallback
keyboardMouse dims  | dims > 2  = threeDControls
                    | otherwise = twoDControls


threeDControls :: IORef Visualisation -> KeyboardMouseCallback
threeDControls vis key Down _ _ = case key of
    (MouseButton LeftButton)    -> vis $~! rotatingVis
    -- (MouseButton WheelDown)     -> vis $~! zoomOutVis
    -- (MouseButton WheelUp)       -> vis $~! zoomInVis
    (Char 'x')                  -> vis $~! viewAlongX
    (Char 'y')                  -> vis $~! viewAlongY
    (Char 'z')                  -> vis $~! viewAlongZ
    (Char 'q')                  -> leaveMainLoop

    _ -> return ()

threeDControls vis key Up _ _ = case key of
    (MouseButton LeftButton)    -> vis $~! notRotatingVis
    _ -> return ()


{- Zooming and Panning Controls and Quit -}
twoDControls :: IORef Visualisation -> KeyboardMouseCallback
twoDControls vis key Down _ _ = case key of
    (MouseButton LeftButton)    -> vis $~! panningVis
    (MouseButton WheelDown)     -> vis $~! zoomOutVis
    (MouseButton WheelUp)       -> vis $~! zoomInVis
    (Char 'r')                  -> vis $~! resetVis
    (Char 'q')                  -> leaveMainLoop
    _ -> return ()

twoDControls vis key Up _ _ = case key of
    (MouseButton LeftButton)    -> vis $~! notPanningVis
    _ -> return ()


{- PANNING and ROTATING -}
mouseMotion :: IORef Visualisation -> IORef Position -> MotionCallback
mouseMotion visRef lastPos curPos = do
    (Vis t g vp) <- get visRef
    if (pan vp) then do
        lPos <- get lastPos
        (_, size) <- get viewport
        let vp' = panView vp lPos curPos size
        writeIORef visRef $ Vis t g vp'
        writeIORef lastPos curPos
    else if (rot vp) then do
        lPos <- get lastPos
        (_, size) <- get viewport
        let vp' = rotView vp lPos curPos size
        writeIORef visRef $ Vis t g vp'
        writeIORef lastPos curPos
    else return ()

{- Update Mouse Position -}
passiveMouseMotion :: IORef Position -> MotionCallback
passiveMouseMotion lastPos curPos = writeIORef lastPos curPos


{- Helper Functions -}
rotatingVis :: Visualisation -> Visualisation
rotatingVis (Vis t g vp) = Vis t g $ rotating vp

rotating :: ViewParams -> ViewParams
rotating (ViewParams ts z _ p) = ViewParams ts z True p

notRotatingVis :: Visualisation -> Visualisation
notRotatingVis (Vis t g vp) = Vis t g $ notRotating vp

notRotating :: ViewParams -> ViewParams
notRotating (ViewParams ts z _ p) = ViewParams ts z False p

panningVis :: Visualisation -> Visualisation
panningVis (Vis t g vp) = Vis t g $ panning vp

panning :: ViewParams -> ViewParams
panning (ViewParams ts z r _) = ViewParams ts z r True

notPanningVis :: Visualisation -> Visualisation
notPanningVis (Vis t g vp) = Vis t g $ notPanning vp

notPanning :: ViewParams -> ViewParams
notPanning (ViewParams ts z r _) = ViewParams ts z r False

zoomInVis :: Visualisation -> Visualisation
zoomInVis (Vis t g vp) = Vis t g $ zoomIn vp

zoomIn :: ViewParams -> ViewParams
zoomIn (ViewParams ts z r p) = ViewParams (t:ts) (z*1.05) r p
    where
        t = scale 1.05 1.05 (1.05::GLfloat)

zoomOutVis :: Visualisation -> Visualisation
zoomOutVis (Vis t g vp) = Vis t g $ zoomOut vp

zoomOut :: ViewParams -> ViewParams
zoomOut (ViewParams ts z r p)
    | z' <= 0   = ViewParams ts z r p
    | otherwise = ViewParams (t:ts) z' r p
    where
        t = scale 0.95 0.95 (0.95::GLfloat)
        z' = (z*0.95)

resetVis :: Visualisation -> Visualisation
resetVis (Vis t g vp) = Vis t g $ reset vp

reset :: ViewParams -> ViewParams
reset _ = ViewParams [] 1 False False

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
    | otherwise     = ViewParams (t++ts++u) z r p
    where
        (x1, y1)    = screenToWorld pos1 size
        (x2, y2)    = screenToWorld pos2 size
        x           = x2-x1
        y           = y2-y1
        h           = sqrt $ (x^2) + (y^2)
        zAngle      = angleToPrimaryAxis x y h
        xAngle      = h * (-90)
        t           = arbitraryRotation zAngle xAngle
        u           = arbitraryRotation zAngle (-xAngle)

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

viewAlongX :: Visualisation -> Visualisation
viewAlongX (Vis t g vp) = Vis t g $ viewAlongXParams vp

viewAlongXParams :: ViewParams -> ViewParams
viewAlongXParams (ViewParams _ z r p) = ViewParams ts z r p
    where
        ts = [
              (rotate (90::GLfloat) $ Vector3 0 1 0)
            , (rotate (-90::GLfloat) $ Vector3 0 1 0)
            ]

viewAlongY :: Visualisation -> Visualisation
viewAlongY (Vis t g vp) = Vis t g $ viewAlongYParams vp

viewAlongYParams :: ViewParams -> ViewParams
viewAlongYParams (ViewParams _ z r p) = ViewParams ts z r p
    where
        ts = [
              (rotate (90::GLfloat) $ Vector3 0 0 1)
            , (rotate (90::GLfloat) $ Vector3 1 0 0)
            , (rotate (-90::GLfloat) $ Vector3 1 0 0)
            , (rotate (-90::GLfloat) $ Vector3 0 0 1)
            ]

viewAlongZ :: Visualisation -> Visualisation
viewAlongZ (Vis t g vp) = Vis t g $ viewAlongZParams vp

viewAlongZParams :: ViewParams -> ViewParams
viewAlongZParams (ViewParams _ z r p) = ViewParams [] z r p