{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Types where

import Graphics.UI.GLUT

data GraphType  = TwoD
                | ThreeD
                deriving Eq

data GraphData  = File String
                | XY ([Float], [Float])
                | XYZ ([Float], [Float], [Float])

type AxisTitles = (String, String)
type GraphTitle = String

data Graph = Graph {
    gType :: GraphType,
    gFunc :: RenderFunction,
    gTitle :: GraphTitle,
    gData :: [GraphData]
}
--                    Points To Render                 Size
type RenderFunction = [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()


data Colour = Red | Green | Blue | White | Grey | Black | Orange

{- A list of colours, starting with background colour,
    followed by colours to use for each data set -}
type ColourScheme = [Colour]

data Vis = Vis {
    graph :: Graph,
    colours :: ColourScheme
}

data ViewParams = ViewParams {
    zoom :: GLfloat,            -- scale factor
    rot :: (GLfloat, GLfloat),  -- angles to rotate by (y axis, x axis)
    pan :: (GLfloat, GLfloat)   -- position to translate vis by
}

convertColour :: Colour -> Color4 GLfloat
convertColour c = case c of
    Types.Red       -> Color4 1 0 0 1
    Types.Green     -> Color4 0 1 0 1
    Types.Blue      -> Color4 0 0 1 1
    Types.White     -> Color4 1 1 1 1
    Types.Grey      -> Color4 0.6 0.6 0.6 1
    Types.Black     -> Color4 0 0 0 1
    Types.Orange    -> Color4 1 0.5 0 1

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
