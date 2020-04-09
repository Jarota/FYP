{-# LANGUAGE RecordWildCards #-}

module Bar2D where

import Graphics.UI.GLUT

import Graph
import DataSet
import ViewParams
import Legend
import Axes
import AxisTicks
import Rendering (renderBars)

format :: Graph -> Graph
format (Bar2D axisLabels _ gData) = Bar2D axisLabels ticks gData'
    where
        gData'  = formatData gData
        ticks   = calcTicks2D gData'

render :: Graph -> ViewParams -> IO ()
render (Bar2D axisLabels ticks gData) ViewParams{..} = do
     -- Axes and labels are stationary
    axes2D
    axisLabels2D axisLabels

    renderLegend gData (0.5,0.9)

    loadIdentity
    sequence transformations
    -- Retrieve resulting modelview matrix for certain offsets
    let modelView = matrix $ Just $ Modelview 0 :: StateVar (GLmatrix GLfloat)
    m <- get modelView
    ts <- getMatrixComponents ColumnMajor m
    let
        y       = -0.7
        z       = head ts
        yDiff   = ((ts!!13) / z) - ( ((y*z) - ((y*z) - y)) / z )
        xStep   = snd $ fst ticks
        w       = xStep / (fromIntegral $ 1 + (length gData))
    
    renderData gData xStep yDiff w

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
        n       = length datasets

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

renderData :: [DataSet] -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderData [] _ _ _             = return ()
renderData (d:ds) xStep yDiff w = do
    renderData' d xStep yDiff w $ fromIntegral $ (length ds) + 1
    renderData ds xStep yDiff w


renderData' :: DataSet -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderData' File{..} _ _ _ _        = return ()
renderData' Raw{..} xStep yDiff w n = do
    color dataColor
    let points = toPoints graphData xStep w n
    renderBars points yDiff $ w/2

toPoints :: [GraphData] -> GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
toPoints [xData,yData] xStep w n = zip3 xs (fst yData) zs
    where
        xs = adjustXpos xStep w n $ fst xData
        zs = repeat (0::GLfloat)

adjustXpos :: GLfloat -> GLfloat -> GLfloat -> [GLfloat] -> [GLfloat]
adjustXpos step w n xs = map ( \x -> x - (step/2) + (w*n) ) xs
