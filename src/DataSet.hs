{-# LANGUAGE RecordWildCards #-}

module DataSet (
    module DataSet,
    module GraphData
)
where

import Graphics.UI.GLUT
import GraphData

data DataSet =
    Raw {
        label       :: String,
        dataColor   :: Color4 GLfloat,
        graphData   :: [GraphData] -- each GraphData corresponds to a dimension
    }
    | File {
        label       :: String,
        dataColor   :: Color4 GLfloat,
        filePath    :: String -- data stored in csv files etc
    }
    deriving Show

{- Helper Functions -}

getXdata :: DataSet -> [GLfloat]
getXdata File{..}   = []
getXdata Raw{..}    = fst $ head graphData

getYdata :: DataSet -> [GLfloat]
getYdata File{..}   = []
getYdata Raw{..}    = fst $ graphData!!1

getZdata :: DataSet -> [GLfloat]
getZdata File{..}   = []
getZdata Raw{..}    = fst $ graphData!!2