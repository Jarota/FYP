{-# LANGUAGE RecordWildCards #-}

module Pie (format, render) where

import Graphics.UI.GLUT

import Graph
import DataSet
import Legend
import ViewParams
import Rendering (renderTriangleFan)


format :: Graph -> Graph
format (Pie gData) = Pie gData'
    where
        gData' = formatData gData

render :: Graph -> ViewParams -> IO ()
render (Pie gData) ViewParams{..} = do
    renderLegend gData (0.625, 0.7)

    loadIdentity
    translate $ Vector3 (-0.1) 0 (0::GLfloat)
    renderData gData 0


{- Formatting Functions -}

formatData :: [DataSet] -> [DataSet]
formatData datasets = map (formatDataSet (2*pi/total)) datasets
    where
        allXs = concatMap getXdata datasets
        total = foldl (+) 0 allXs

-- Convert the value to a proportional angle
formatDataSet :: GLfloat -> DataSet -> DataSet
formatDataSet a (Raw c l gData) = Raw c l [gData']
    where
        gData' = fitGraphData a $ head gData

fitGraphData :: GLfloat -> GraphData -> GraphData
fitGraphData a (xs, ss) = (xs', ss)
    where
        xs' = map (*a) xs


{- Rendering Functions -}

renderData :: [DataSet] -> GLfloat -> IO ()
renderData [] _     = return ()
renderData (d:ds) a = do
    renderData' d a
    renderData ds $ a + (getAngle d)

renderData' :: DataSet -> GLfloat -> IO ()
renderData' File{..} _ = return ()
renderData' Raw{..} a  = do
    color dataColor
    let points = toPoints graphData a
    renderTriangleFan points

toPoints :: [GraphData] -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
toPoints [angleData] a = zip3 (0:xs) (0:ys) zs
    where
        theta = head $ fst angleData
        xs = interpolateX a a (a+theta)
        ys = interpolateY a a (a+theta)
        zs = repeat (0::GLfloat)

-- current, final, result
interpolateX :: GLfloat -> GLfloat -> GLfloat -> [GLfloat]
interpolateX current start end  | current > end                 = []
                                | otherwise                     = ((cos current)/2):(interpolateX (current+0.001) start end)

interpolateY :: GLfloat -> GLfloat -> GLfloat -> [GLfloat]
interpolateY current start end  | current > end                 = []
                                | otherwise                     = ((sin current)/2):(interpolateY (current+0.001) start end)


{- Helper Functions -}

getAngle :: DataSet -> GLfloat
getAngle Raw{..} = head $ fst $ head graphData
getAngle _       = 0