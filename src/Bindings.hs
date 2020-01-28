module Bindings (reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef
import Display

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> KeyboardMouseCallback
keyboardMouse zoom key Down _ _ = case key of
    (MouseButton WheelDown) -> zoom $~! (\z -> if z <= 0 then z else z - 0.1)
    (MouseButton WheelUp)   -> zoom $~! (\z -> z + 0.1)
    (Char 'q')              -> leaveMainLoop
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
