module Graph where

import Graphics.UI.GLUT (Color4, GLfloat)

import DataSet
import AxisTicks (TickInfo)

data Graph
    = Scatter2D (String,String) (TickInfo, TickInfo) [DataSet]
    | Scatter3D (String,String,String) (TickInfo, TickInfo, TickInfo) [DataSet]
    | Bar2D (String,String) (TickInfo, TickInfo) [DataSet]
    | Pie [DataSet]
    deriving Show

dimensions :: Graph -> Int
dimensions (Scatter3D _ _ _) = 3
dimensions _ = 2