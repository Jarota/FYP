{-# LANGUAGE RecordWildCards #-}

module Visualisation (Visualisation(..), formatVis, renderVis) where

import Graphics.UI.GLUT

import Rendering
import ViewParams
import Graph
import qualified Scatter2D as S2D

data Visualisation = Vis {
    title :: String,
    graph :: Graph,
    viewParams :: ViewParams
} deriving Show

renderVis :: Visualisation -> IO ()
renderVis (Vis title graph viewParams) = do
    renderTitle title
    renderFrame
    preservingMatrix $ do
        renderGraph graph viewParams

renderGraph :: Graph -> ViewParams -> IO ()
renderGraph g@(Scatter2D _ _ _) vp = S2D.render g vp

formatVis :: Visualisation -> Visualisation
formatVis (Vis t g vp) = Vis t (formatGraph g) vp

formatGraph :: Graph -> Graph
formatGraph g@(Scatter2D _ _ _) = S2D.format g
