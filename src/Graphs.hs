module Graphs where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Types
import Cube

getGraphType :: Graph x y -> GraphType
getGraphType (Graph _ graphType _) = graphType

renderGraph :: Graph x y -> [Colour] -> DisplayCallback
renderGraph g cs    | graphType == ScatterPlot2D    = renderScatter2D g cs
                    | graphType == BarChart         = renderScatter2D g cs
                    where
                        graphType = getGraphType g

renderScatter2D :: Graph x y -> [Colour] -> DisplayCallback
renderScatter2D _ [] = return ()
renderScatter2D graph (c:cs) = do
        color $ convertColour c
        cube 0.1
        renderScatter2D graph cs
