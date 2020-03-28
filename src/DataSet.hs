module DataSet where

import Graphics.UI.GLUT
import GraphData

data DataSet =
    Raw {
        color       :: Color4 GLfloat,
        label       :: String,
        graphData   :: [GraphData] -- as many GraphData items as dimensions of the graph
    }
    | File {
        color       :: Color4 GLfloat,
        label       :: String,
        filePath    :: String -- data stored in csv files etc
    }
