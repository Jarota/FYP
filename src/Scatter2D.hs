{-# LANGUAGE RecordWildCards #-}

module Scatter2D where

import Graphics.UI.GLUT

import Graph
import DataSet
import ViewParams
import Axes
import AxisTicks
import Rendering

--                          Axis Labels      Data
data Scatter2D = Scatter2D (String,String) [DataSet]

instance Graph Scatter2D where

    format g = g -- TODO (parse data from csv files etc)
    
    render (Scatter2D axisLabels gData) ViewParams{..} = do
        -- Pan, Zoom, and render data
        sequence transformations
        -- TODO zoom?
        -- render' gData -- TODO
        
        -- retrieve resulting modelview matrix for proper axis tick offsets
        let modelView = matrix $ Just $ Modelview 0 :: StateVar (GLmatrix GLfloat)
        m <- get modelView
        ts <- getMatrixComponents ColumnMajor m
        -- let (ticksX, ticksY) = axisTicks2D gData (ts!!12, ts!!13, ts!!14)

        -- render the axes, ticks, and labels
        preservingMatrix $ do
            loadIdentity
            axes2D
            -- renderTicks2D ticksX ticksY
            axisLabels2D axisLabels