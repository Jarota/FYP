module Graph where

import Graphics.UI.GLUT (Color4, GLfloat)

import DataSet
import AxisTicks (TickInfo)

data Graph
    = Scatter2D (String,String) (TickInfo, TickInfo) [DataSet]
    deriving Show

dimensions :: Graph -> Int
dimensions (Scatter2D _ _ _) = 2