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
render2D Graph{..} (c:cs) ViewParams{..} = do
    renderTitle gTitle
    color $ convertColour c
    redoBackground bg2D
    
    preservingMatrix $ do
        -- Pan, Zoom, and render data
        sequence transformations
        let gData' = map (zoomData zoom) gData
        render2D' gData' cs gFunc
        
        -- retrieve resulting modelview matrix for proper axis tick offsets
        let modelView = matrix $ Just $ Modelview 0 :: StateVar (GLmatrix GLfloat)
        m <- get modelView
        ts <- getMatrixComponents ColumnMajor m
        let (ticksX, ticksY) = axisTicks2D gData' (ts!!12, ts!!13, ts!!14)

        -- render the axes, ticks, and labels
        preservingMatrix $ do
            loadIdentity
            axes2D
            renderTicks2D ticksX ticksY
            axisLabels2D gAxes


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
render3D Graph{..} (c:cs) ViewParams{..} = do
    renderTitle gTitle
    scale 0.7 0.7 (0.7 :: GLfloat)
    preservingMatrix $ do
        sequence transformations
        render3D' gData' cs gFunc
        axes3D
        renderTicks3D ticksX ticksY ticksZ
    where
        gData' = map (zoomData zoom) gData
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

tickStepAndOffset :: [GLfloat] -> GLfloat -> (GLfloat, GLfloat)
tickStepAndOffset [] _ = (1, 1)
tickStepAndOffset xs t = (step, offset)
    where
        step    = minDifference xs
        minX    = abs $ minimum xs
        offset  = adjustOffset (minX-t) step

adjustOffset :: GLfloat -> GLfloat -> GLfloat
adjustOffset offset step    | offset >= 0.8 = offset
                            | otherwise     = adjustOffset (offset+step) step

minDifference :: (Num a, Ord a) => [a] -> a
minDifference xs    | length xs == 1    = 1
                    | otherwise         = minimum $ map abs $ zipWith (-) xs (drop 1 xs)

axisTicks2D :: [GraphData] -> (GLfloat, GLfloat, GLfloat) -> ((GLfloat, GLfloat), (GLfloat, GLfloat))
axisTicks2D gData (x, y, z) = (minXs, minYs)
    where
        xs      = map (getXTicks x) gData
        minXs   = smallestStep xs $ head xs
        ys      = map (getYTicks y) gData
        minYs   = smallestStep ys $ head ys

axisTicks3D :: [GraphData] -> ((GLfloat, GLfloat), (GLfloat, GLfloat), (GLfloat, GLfloat))
axisTicks3D gData = (minXs, minYs, minZs)
    where
        xs      = map (getXTicks 0) gData
        minXs   = smallestStep xs $ head xs
        ys      = map (getYTicks 0) gData
        minYs   = smallestStep ys $ head ys
        zs      = map (getZTicks 0) gData
        minZs   = smallestStep zs $ head zs

getXTicks :: GLfloat -> GraphData -> (GLfloat, GLfloat)
getXTicks t (XY (xs, _))      = tickStepAndOffset xs t
getXTicks t (XYZ (xs, _, _))  = tickStepAndOffset xs t

getYTicks :: GLfloat -> GraphData -> (GLfloat, GLfloat)
getYTicks t (XY (_, ys))      = tickStepAndOffset ys t
getYTicks t (XYZ (_, ys, _))  = tickStepAndOffset ys t

getZTicks :: GLfloat -> GraphData -> (GLfloat, GLfloat)
getZTicks t (XYZ (_, _, zs))  = tickStepAndOffset zs t

smallestStep :: [(GLfloat, GLfloat)] -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
smallestStep [] ticks = ticks
smallestStep ((s',o'):rest) (s,o)
    | s' < s    = smallestStep rest (s',o')
    | otherwise = smallestStep rest (s,o)

zoomData :: GLfloat -> GraphData -> GraphData
zoomData z (XY (xs, ys)) = XY (xs', ys')
    where
        xs' = map (*z) xs
        ys' = map (*z) ys

zoomData z (XYZ (xs, ys, zs)) = XYZ (xs', ys', zs')
    where
        xs' = map (*z) xs
        ys' = map (*z) ys
        zs' = map (*z) zs

zoomData _ _ = File " "