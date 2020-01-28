module Graphs (renderGraph) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Types
import Rendering

renderGraph :: Graph GLfloat GLfloat -> [Colour] -> DisplayCallback
renderGraph (Scatter2D title ds) cs = renderScatter2D (Scatter2D title ds) cs

renderTitle :: String -> IO ()
renderTitle _ = return () -- TODO

renderScatter2D :: Graph GLfloat GLfloat -> [Colour] -> DisplayCallback
renderScatter2D (Scatter2D title ds) colours    = do
    renderTitle title
    axes2D
    renderScatter2D' ds colours

renderScatter2D' :: [GraphData GLfloat GLfloat] -> [Colour] -> IO ()
renderScatter2D' [] _           = return ()
renderScatter2D' (d:ds) (c:cs)  = do
    color $ convertColour c
    renderScatter2DData d
    -- square 0.3
    renderScatter2D' ds cs

renderScatter2DData :: GraphData GLfloat GLfloat -> IO ()
renderScatter2DData (File _)        = return () -- RenderTODO
renderScatter2DData (Raw (xs, ys))  = renderSquares ps'
    where
        maxX    = maximum xs
        stepX   = 1.6 / maxX
        xs'     = map (\x -> -0.8 + (x * stepX)) xs
        maxY    = maximum ys
        stepY   = 1.6 / maxY
        ys'     = map (\y -> -0.8 + (y * stepY)) ys
        zs      = take (length xs') (repeat (0::GLfloat))
        ps      = zip3 xs' ys' zs
        ps'     = concatMap (pointToSquare 0.02) ps

axes2D :: IO ()
axes2D = do
    color $ convertColour Types.White
    renderPrimitive Lines $ mapM_ vertex3f
        [ (-0.8, 0.8, 0), (-0.8, -0.8, 0),
        (-0.8, -0.8, 0), (0.8, -0.8, 0) ]
