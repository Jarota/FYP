{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Graphs (renderGraph) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Types
import Rendering
import Axes

renderGraph :: Graph -> [Colour] -> ViewParams -> DisplayCallback
renderGraph Graph{..} cs vp | gType == Scatter2D    = render2D gTitle gData cs vp renderSquares
                            | gType == Scatter3D    = render3D gTitle gData cs vp renderCubes
                            | gType == Bar2D        = render2D gTitle gData cs vp renderBars

{- 2D FUNCTIONS -}

render2D :: String -> [GraphData] -> [Colour] -> ViewParams -> RenderFunction -> DisplayCallback
render2D title ds (c:cs) viewParams dataFunc = do
    renderTitle title
    axes2D
    color $ convertColour c
    render2D' ds cs viewParams dataFunc

render2D' :: [GraphData] -> [Colour] -> ViewParams -> RenderFunction -> IO ()
render2D' [] _ _ _                  = return ()
render2D' (d:ds) (c:cs) vp dataFunc = do
    renderData2D d c vp dataFunc
    render2D' ds cs vp dataFunc

renderData2D :: GraphData -> Colour -> ViewParams -> RenderFunction -> IO ()
renderData2D (XY (xs, ys)) c vp dataFunc  = do
    renderTicks2D ticksX ticksY
    color $ convertColour c
    dataFunc ps $ (minDifference xs') * 0.35
    where
        z       = zoom vp
        xs'     = fitData xs
        xs''    = map (*z) xs'
        ys'     = fitData ys
        ys''    = map (*z) ys'
        ps      = zip3 xs'' ys'' $ repeat (-0.8::GLfloat)
        ticksX  = tickStepAndOffset xs''
        ticksY  = tickStepAndOffset ys''

renderData2D _ _ _ _ = return ()

{- 3D FUNCTIONS -}

render3D :: String -> [GraphData] -> [Colour] -> ViewParams -> RenderFunction -> DisplayCallback
render3D title ds (c:cs) viewParams dataFunc = do
    renderTitle title
    rotateView degrees
    scale 0.7 0.7 (0.7 :: GLfloat)
    render3D' ds cs viewParams dataFunc
    where
        degrees = rot viewParams

render3D' :: [GraphData] -> [Colour] -> ViewParams -> RenderFunction -> IO ()
render3D' [] _ _ _                  = return ()
render3D' (d:ds) (c:cs) vp dataFunc = do
    renderData3D d c vp dataFunc
    render3D' ds cs vp dataFunc

renderData3D :: GraphData -> Colour -> ViewParams -> RenderFunction -> IO ()
renderData3D (XYZ (xs, ys, zs)) c vp dataFunc  = do
    color $ convertColour c
    dataFunc ps $ (minDifference xs') * 0.3
    axes3D
    renderTicks3D ticksX ticksY ticksZ
    where
        z       = zoom vp
        xs'     = fitData xs
        xs''    = map (*z) xs'
        ys'     = map (*z) $ fitData ys
        zs'     = map (*z) $ fitData zs
        ps      = zip3 xs'' ys' zs'
        ticksX  = tickStepAndOffset xs''
        ticksY  = tickStepAndOffset ys'
        ticksZ  = tickStepAndOffset zs'

renderData3D _ _ _ _ = return ()

{- HELPER FUNCTIONS -}

fitData :: [GLfloat] -> [GLfloat]
fitData xs = map (\x -> -0.8 + (x * step)) xs
    where
        range   = maximum xs
        step    = 1.6 / range

tickStepAndOffset :: [GLfloat] -> (GLfloat, GLfloat)
tickStepAndOffset xs = (step, offset)
    where
        step    = minDifference xs
        minX    = abs $ minimum xs
        offset  = adjustOffset minX step

adjustOffset :: GLfloat -> GLfloat -> GLfloat
adjustOffset offset step    | offset >= 0.8 = offset
                            | otherwise     = adjustOffset (offset+step) step

minDifference :: (Num a, Ord a) => [a] -> a
minDifference xs = minimum $ map abs $ zipWith (-) xs (drop 1 xs)

rotateView :: (GLfloat, GLfloat) -> IO ()
rotateView (x, y) = do
    rotate x $ Vector3 1 0 0
    rotate y $ Vector3 0 1 0
