module Bindings (reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Display
import Types

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef ViewParams -> KeyboardMouseCallback
keyboardMouse vp key Down _ _ = case key of
    (MouseButton WheelDown) -> vp $~! zoomOut
    (MouseButton WheelUp)   -> vp $~! zoomIn
    (Char 'q')              -> leaveMainLoop
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()


zoomIn :: ViewParams -> ViewParams
zoomIn vp = ViewParams z (pan vp) (rot vp)
    where
        z = (zoom vp) + 0.05

zoomOut :: ViewParams -> ViewParams
zoomOut vp = let z = zoom vp in
    if z <= 0 then
        ViewParams z (pan vp) (rot vp)
    else
        ViewParams (z - 0.05) (pan vp) (rot vp)
