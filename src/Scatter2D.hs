{-# LANGUAGE RecordWildCards #-}

module Scatter2D (format, render) where

import Graphics.UI.GLUT

import Graph
import DataSet
import ViewParams
import Axes
import AxisTicks
import Legend
import Rendering (renderSquares)

format :: Graph -> Graph
format (Scatter2D axisLabels _ gData) = Scatter2D axisLabels ticks gData'
    where
        gData'  = formatData gData
        ticks   = calcTicks2D gData'

render :: Graph -> ViewParams -> IO ()
render (Scatter2D axisLabels ticks gData) ViewParams{..} = do

    -- Legen, axes and labels are stationary
    axes2D
    axisLabels2D axisLabels
    renderLegend gData (0.63,0.73)
    loadIdentity
    -- Zoom, pan, and render data
    sequence transformations
    renderData gData
        
    -- Retrieve resulting modelview matrix for axis tick offsets
    let modelView = matrix $ Just $ Modelview 0 :: StateVar (GLmatrix GLfloat)
    m <- get modelView
    ts <- getMatrixComponents ColumnMajor m
    let z = head ts

    loadIdentity
    renderXticks (zoomTickInfo z $ fst ticks) (ts!!12)
    renderYticks (zoomTickInfo z $ snd ticks) (ts!!13)


{- Formatting Functions -}

formatData :: [DataSet] -> [DataSet]
formatData datasets = map (formatDataSet stepX stepY) datasets
    where
        allXs   = concatMap getXdata datasets
        allYs   = concatMap getYdata datasets
        stepX   = 1.3 / maximum allXs
        stepY   = 1.3 / maximum allYs

formatDataSet :: GLfloat -> GLfloat -> DataSet -> DataSet
formatDataSet stepX stepY (Raw c l gData) = Raw c l gData'
    where
        xData   = fitGraphData stepX $ head gData
        yData   = fitGraphData stepY $ gData!!1
        gData'  = [xData,yData]

fitGraphData :: GLfloat -> GraphData -> GraphData
fitGraphData step (xs, ss) = (xs', ss)
    where
        xs' = map (fit step) xs

fit :: GLfloat -> GLfloat -> GLfloat
fit step x = -0.7 + (x * step)


{- Rendering Functions -}

renderData :: [DataSet] -> IO ()
renderData []       = return ()
renderData (d:ds)   = do
    renderData' d
    renderData ds

renderData' :: DataSet -> IO ()
renderData' File{..}   = return ()
renderData' Raw{..}    = do
    color dataColor
    let points = toPoints graphData
    renderSquares points 0.025

toPoints :: [GraphData] -> [(GLfloat, GLfloat, GLfloat)]
toPoints (xData:(yData:[])) = zip3 (fst xData) (fst yData) zs
    where
        zs = repeat (0::GLfloat)
