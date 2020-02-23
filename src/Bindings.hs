module Bindings (reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Types

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef ViewParams -> IORef Position -> KeyboardMouseCallback
keyboardMouse vp lastPos LeftButton Up _ curPos = do
    writeIORef lastPos (Position (-1) (-1))
keyboardMouse vp lastPos LeftButton Down _ curPos = do
    lPos <- get lastPos
    if lPos == (Position (-1) (-1)) then
        writeIORef lastPos curPos
    else do
        let diff = getDiff lPos curPos
        vp' <- get vp
        let vp'' = panView vp' diff
        writeIORef vp vp''



keyboardMouse vp _ key Down _ _ = case key of
    (MouseButton WheelDown) -> vp $~! zoomOut
    (MouseButton WheelUp)   -> vp $~! zoomIn
    (SpecialKey KeyUp)      -> vp $~! rotUp
    (SpecialKey KeyDown)    -> vp $~! rotDown
    (SpecialKey KeyLeft)    -> vp $~! rotLeft
    (SpecialKey KeyRight)   -> vp $~! rotRight
    (Char 'q')              -> leaveMainLoop
    _ -> return ()

keyboardMouse _ _ _ _ _ = return ()


{- Helper Functions -}

zoomIn :: ViewParams -> ViewParams
zoomIn vp = ViewParams z (rot vp) (pan vp)
    where
        z = (zoom vp) + 0.05

zoomOut :: ViewParams -> ViewParams
zoomOut vp = let z = zoom vp in
    if z <= 0 then
        ViewParams z (rot vp) (pan vp)
    else
        ViewParams (z - 0.05) (rot vp) (pan vp)

rotUp :: ViewParams -> ViewParams
rotUp vp = let (x, y) = rot vp in
    ViewParams (zoom vp) (x+5, y) (pan vp)

rotDown :: ViewParams -> ViewParams
rotDown vp = let (x, y) = rot vp in
    ViewParams (zoom vp) (x-5, y) (pan vp)

rotLeft :: ViewParams -> ViewParams
rotLeft vp = let (x, y) = rot vp in
    ViewParams (zoom vp) (x, y+5) (pan vp)

rotRight :: ViewParams -> ViewParams
rotRight vp = let (x, y) = rot vp in
    ViewParams (zoom vp) (x, y-5) (pan vp)

getDiff :: Position -> Position -> Position
getDiff (Position x1 y1) (Position x2 y2) = Position (x2-x1) (y2-y1)

panView :: ViewParams -> Position -> ViewParams
