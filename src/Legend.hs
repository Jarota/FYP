module Legend where

import Graphics.UI.GLUT

import DataSet

--              Data            Position of Legend
renderLegend :: [DataSet] -> (GLfloat, GLfloat, GLfloat) -> IO ()
renderLegend _ _ = return ()