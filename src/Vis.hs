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
        stepX   = 1.6 / (maximum $ concat $ map getXs gData)
        stepY   = 1.6 / (maximum $ concat $ map getYs gData)
        stepZ   = 1.6 / (maximum $ concat $ map getZs gData)
        gData'  = map (fitData stepX stepY stepZ) gData

getXs :: GraphData -> [GLfloat]
getXs (XY (xs, _)) = xs
getXs (XYZ (xs, _, _)) = xs

getYs :: GraphData -> [GLfloat]
getYs (XY (_, ys)) = ys
getYs (XYZ (_, ys, _)) = ys

getZs :: GraphData -> [GLfloat]
getZs (XYZ (_, _, zs)) = zs

fitData :: GLfloat -> GLfloat -> GLfloat -> GraphData -> GraphData
fitData stepX stepY _ (XY (xs, ys)) = XY (xs', ys')
    where
        xs'     = map (\x -> -0.8 + (x * stepX)) xs
        ys'     = map (\x -> -0.8 + (x * stepY)) ys

fitData stepX stepY stepZ (XYZ (xs, ys, zs)) = XYZ (xs', ys', zs')
    where
        xs'     = map (\x -> -0.8 + (x * stepX)) xs
        ys'     = map (\x -> -0.8 + (x * stepY)) ys
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
