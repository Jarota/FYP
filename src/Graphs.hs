module Graphs (renderGraph) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Types
import Rendering

renderGraph :: Graph GLfloat GLfloat -> [Colour] -> ViewParams -> DisplayCallback
renderGraph (Scatter2D title ds) cs vp = renderScatter2D (Scatter2D title ds) cs vp

renderScatter2D :: Graph GLfloat GLfloat -> [Colour] -> ViewParams -> DisplayCallback
renderScatter2D (Scatter2D title ds) (c:cs) viewParams = do
    renderTitle title
    axes2D
    color $ convertColour c
    redoBackground
    renderScatter2D' ds cs viewParams

renderScatter2D' :: [GraphData GLfloat GLfloat] -> [Colour] -> ViewParams -> IO ()
renderScatter2D' [] _ _             = return ()
renderScatter2D' (d:ds) (c:cs) vp   = do
    renderScatter2DData d c vp
    renderScatter2D' ds cs vp

renderScatter2DData :: GraphData GLfloat GLfloat -> Colour -> ViewParams -> IO ()
renderScatter2DData (File _) _ _        = return () -- TODO
renderScatter2DData (Raw (xs, ys)) c vp = do
    renderTicks ticksX ticksY
    color $ convertColour c
    renderSquares ps'
    where
        z       = zoom vp
        xs'     = fitData xs z
        ys'     = fitData ys z
        zs      = repeat (0::GLfloat)
        ps      = zip3 xs' ys' zs
        ps'     = concatMap (pointToSquare 0.02) ps
        ticksX  = tickStepAndOffset xs'
        ticksY  = tickStepAndOffset ys'

fitData :: [GLfloat] -> GLfloat -> [GLfloat]
fitData xs z = map (\x -> (-0.8 + (x * step)) * z) xs
    where
        range   = maximum xs
        step    = 1.6 / range

tickStepAndOffset :: [GLfloat] -> (GLfloat, GLfloat)
tickStepAndOffset xs = (step, offset)
    where
        step    = minDifference xs
        minX    = abs $ minimum xs
        extra   = (abs $ minX - 0.8) / step
        offset  = minX -- + (extra * (step + 1))


minDifference :: (Num a, Ord a) => [a] -> a
minDifference xs = minimum $ map abs $ zipWith (-) xs (drop 1 xs)
