{-# LANGUAGE RecordWildCards #-}

module Scatter3D (format, render) where

import Graphics.UI.GLUT

import Graph
import DataSet
import Legend
import ViewParams
import Axes
import AxisTicks
import Rendering (renderCubes)

format :: Graph -> Graph
format (Scatter3D axisLabels _ gData) = Scatter3D axisLabels ticks gData'
    where
        gData'  = formatData gData
        ticks   = calcTicks3D gData'

render :: Graph -> ViewParams -> IO ()
render (Scatter3D axisLabels ticks gData) ViewParams{..} = do

    renderLegend gData (0.625, 0.7)

    loadIdentity
    scale 0.6 0.6 (0.6::GLfloat)
    let half = round $ (fromIntegral $ length transformations) / 2
    sequence $ take half transformations
    renderData gData
    axes3D
    renderTicks3D ticks
    axisLabels3D axisLabels $ drop half transformations



{- Formatting Functions -}

formatData :: [DataSet] -> [DataSet]
formatData datasets = map (formatDataSet stepX stepY stepZ) datasets
    where
        allXs   = concatMap getXdata datasets
        allYs   = concatMap getYdata datasets
        allZs   = concatMap getZdata datasets
        stepX   = 1.3 / maximum allXs
        stepY   = 1.3 / maximum allYs
        stepZ   = 1.3 / maximum allZs

formatDataSet :: GLfloat -> GLfloat -> GLfloat -> DataSet -> DataSet
formatDataSet stepX stepY stepZ (Raw c l gData) = Raw c l gData'
    where
        xData   = fitGraphData stepX $ head gData
        yData   = fitGraphData stepY $ gData!!1
        zData   = fitGraphData stepZ $ gData!!2
        gData'  = [xData,yData,zData]

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
    renderCubes points 0.02

toPoints :: [GraphData] -> [(GLfloat, GLfloat, GLfloat)]
toPoints [xData,yData,zData] = zip3 (fst xData) (fst yData) (fst zData)
