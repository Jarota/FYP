{-# LANGUAGE RecordWildCards #-}

module Vis (renderVis, visType, fitVisData, getVisPaths, replaceVisPaths) where

import Graphics.UI.GLUT
import Control.Monad

import Types
import Graphs

renderVis :: Vis -> ViewParams -> IO ()
renderVis (Vis graph cs) viewParams = do
        clearColor $= (convertColour $ head cs)
        clear [ColorBuffer, DepthBuffer]
        renderGraph graph cs viewParams

fitVisData :: Vis -> Vis
fitVisData (Vis graph cs) = Vis (fitGraphData graph) cs

fitGraphData :: Graph -> Graph
fitGraphData Graph{..} = Graph gType gFunc gTitle gData'
    where
        gData' = map fitData gData

fitData :: GraphData -> GraphData
fitData (XY (xs, ys)) = XY (xs', ys')
    where
        rangeX  = maximum xs
        stepX   = 1.6 / rangeX
        xs'     = map (\x -> -0.8 + (x * stepX)) xs
        rangeY  = maximum ys
        stepY   = 1.6 / rangeY
        ys'     = map (\x -> -0.8 + (x * stepY)) ys

fitData (XYZ (xs, ys, zs)) = XYZ (xs', ys', zs')
    where
        rangeX  = maximum xs
        stepX   = 1.6 / rangeX
        xs'     = map (\x -> -0.8 + (x * stepX)) xs
        rangeY  = maximum ys
        stepY   = 1.6 / rangeY
        ys'     = map (\x -> -0.8 + (x * stepY)) ys
        rangeZ  = maximum zs
        stepZ   = 1.6 / rangeZ
        zs'     = map (\x -> -0.8 + (x * stepZ)) zs


getVisPaths :: Vis -> [String]
getVisPaths Vis{..} = getGraphPaths graph

getGraphPaths :: Graph -> [String]
getGraphPaths Graph{..} = getDataPaths gData

getDataPaths :: [GraphData] -> [String]
getDataPaths []                 = []
getDataPaths (( File path ):ds) = path:(getDataPaths ds)
getDataPaths (d:ds)             = getDataPaths ds

replaceVisPaths :: Vis -> [GraphData] -> Vis
replaceVisPaths Vis{..} parsedData = Vis newGraph colours
    where
        newGraph = replaceGraphPaths graph parsedData

replaceGraphPaths :: Graph -> [GraphData] -> Graph
replaceGraphPaths Graph{..} parsedData = Graph gType gFunc gTitle newData
    where
        newData = replaceDataPaths gData parsedData

--                  Current        Parsed         Updated
replaceDataPaths :: [GraphData] -> [GraphData] -> [GraphData]
replaceDataPaths ds []                  = ds
replaceDataPaths (( File _ ):ds) (p:ps) = [p] ++ (replaceDataPaths ds ps)
replaceDataPaths (d:ds) ps              = replaceDataPaths ds ps

visType :: Vis -> GraphType
visType ( Vis Graph{..} _ ) = gType
