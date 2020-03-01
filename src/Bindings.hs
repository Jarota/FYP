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
    (MouseButton RightButton)   -> vp $~! rotating
    (MouseButton LeftButton)    -> vp $~! panning
    (MouseButton WheelDown)     -> vp $~! zoomOut
    (MouseButton WheelUp)       -> vp $~! zoomIn
    (Char 'q')                  -> leaveMainLoop

    _ -> return ()

threeDControls vp key Up _ _ = case key of
    (MouseButton RightButton)   -> vp $~! notRotating
    (MouseButton LeftButton)    -> vp $~! notPanning
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
        let diff = getDiff lPos curPos
        let vp'' = panView vp' diff
        writeIORef vp vp''
        writeIORef lastPos curPos
    else if (rot vp') then do
        lPos <- get lastPos
        let vp'' = rotView vp' lPos curPos
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

getDiff :: Position -> Position -> Position
getDiff (Position x1 y1) (Position x2 y2) = Position (x2-x1) (y2-y1)

panView :: ViewParams -> Position -> ViewParams
panView (ViewParams rs ts r p) (Position x y) = ViewParams rs (t:ts) r p
    where
        x'  = (fromIntegral x)/800
        y'  = (fromIntegral y)/800
        t   = translate $ Vector3 x' (-y') (0::GLfloat)

rotView :: ViewParams -> Position -> Position -> ViewParams
rotView (ViewParams rs ts r p) (Position x1 y1) (Position x2 y2) = ViewParams (t:rs) (t:ts) r p
    where
        t = rotate (10 :: GLfloat) $ Vector3 0 1 0
