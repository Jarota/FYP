{-# LANGUAGE RecordWildCards #-}

module Graphs (renderGraph) where

import Graphics.UI.GLUT hiding (TwoD, ThreeD)
import Control.Monad
import Data.IORef
import Types
import Rendering
import Axes

renderGraph :: Graph -> [Colour] -> ViewParams -> DisplayCallback
renderGraph Graph{..} cs vp | gType == TwoD     = render2D Graph{..} cs vp
                            | gType == ThreeD   = render3D Graph{..} cs vp

{- 2D FUNCTIONS -}

render2D :: Graph -> [Colour] -> ViewParams -> DisplayCallback
render2D Graph{..} (c:cs) vp = do
    renderTitle gTitle
    axisLabels2D gAxes
    axes2D
    renderTicks2D ticksX ticksY
    color $ convertColour c
    redoBackground bg2D
    render2D' gData' cs gFunc
    where
        gData'              = map (zoomData vp) gData
        (ticksX, ticksY)    = axisTicks2D gData'

render2D' :: [GraphData] -> [Colour] -> RenderFunction -> IO ()
render2D' [] _ _                 = return ()
render2D' (d:ds) (c:cs) dataFunc = do
    renderData2D d c dataFunc
    render2D' ds cs dataFunc

renderData2D :: GraphData -> Colour -> RenderFunction -> IO ()
renderData2D (XY (xs, ys)) c dataFunc = do
    color $ convertColour c
    dataFunc ps $ (minDifference xs) * 0.3
    where
        ps = zip3 xs ys $ repeat (0::GLfloat)
renderData2D _ _ _ = return ()


{- 3D FUNCTIONS -}

render3D :: Graph -> [Colour] -> ViewParams -> DisplayCallback
render3D Graph{..} (c:cs) vp = do
    renderTitle gTitle
    rotateView degrees
    scale 0.7 0.7 (0.7 :: GLfloat)
    axisLabels3D gAxes degrees
    axes3D
    renderTicks3D ticksX ticksY ticksZ
    render3D' gData' cs gFunc
    where
        degrees = rot vp
        gData'  = map (zoomData vp) gData
        (ticksX, ticksY, ticksZ) = axisTicks3D gData'

render3D' :: [GraphData] -> [Colour] -> RenderFunction -> IO ()
render3D' [] _ _                 = return ()
render3D' (d:ds) (c:cs) dataFunc = do
    renderData3D d c dataFunc
    render3D' ds cs dataFunc

renderData3D :: GraphData -> Colour -> RenderFunction -> IO ()
renderData3D (XYZ (xs, ys, zs)) c dataFunc = do
    color $ convertColour c
    dataFunc ps $ (minDifference xs) * 0.3
    where
        ps = zip3 xs ys zs
renderData3D _ _ _ = return ()


{- HELPER FUNCTIONS -}

zoomData :: ViewParams -> GraphData -> GraphData
zoomData vp (XY (xs, ys)) = XY (xs', ys')
    where
        z   = zoom vp
        xs' = map (*z) xs
        ys' = map (*z) ys
zoomData vp (XYZ (xs, ys, zs)) = XYZ (xs', ys', zs')
    where
        z   = zoom vp
        xs' = map (*z) xs
        ys' = map (*z) ys
        zs' = map (*z) zs

rotateView :: (GLfloat, GLfloat) -> IO ()
rotateView (x, y) = do
    rotate x $ Vector3 1 0 0
    rotate y $ Vector3 0 1 0

tickStepAndOffset :: [GLfloat] -> (GLfloat, GLfloat)
tickStepAndOffset [] = (1, 1)
tickStepAndOffset xs = (step, offset)
    where
        step    = minDifference xs
        minX    = abs $ minimum xs
        offset  = adjustOffset minX step

adjustOffset :: GLfloat -> GLfloat -> GLfloat
adjustOffset offset step    | offset >= 0.8 = offset
                            | otherwise     = adjustOffset (offset+step) step

minDifference :: (Num a, Ord a) => [a] -> a
minDifference xs    | length xs == 1    = 1
                    | otherwise         = minimum $ map abs $ zipWith (-) xs (drop 1 xs)

axisTicks2D :: [GraphData] -> ((GLfloat, GLfloat), (GLfloat, GLfloat))
axisTicks2D gData = (minXs, minYs)
    where
        xs      = map getXTicks gData
        minXs   = smallestStep xs $ head xs
        ys      = map getYTicks gData
        minYs   = smallestStep ys $ head ys

axisTicks3D :: [GraphData] -> ((GLfloat, GLfloat), (GLfloat, GLfloat), (GLfloat, GLfloat))
axisTicks3D gData = (minXs, minYs, minZs)
    where
        xs      = map getXTicks gData
        minXs   = smallestStep xs $ head xs
        ys      = map getYTicks gData
        minYs   = smallestStep ys $ head ys
        zs      = map getZTicks gData
        minZs   = smallestStep zs $ head zs

getXTicks :: GraphData -> (GLfloat, GLfloat)
getXTicks (XY (xs, _))      = tickStepAndOffset xs
getXTicks (XYZ (xs, _, _))  = tickStepAndOffset xs

getYTicks :: GraphData -> (GLfloat, GLfloat)
getYTicks (XY (_, ys))      = tickStepAndOffset ys
getYTicks (XYZ (_, ys, _))  = tickStepAndOffset ys

getZTicks :: GraphData -> (GLfloat, GLfloat)
getZTicks (XYZ (_, _, zs))  = tickStepAndOffset zs

smallestStep :: [(GLfloat, GLfloat)] -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
smallestStep [] ticks = ticks
smallestStep ((s',o'):rest) (s,o)
    | s' < s    = smallestStep rest (s',o')
    | otherwise = smallestStep rest (s,o)
