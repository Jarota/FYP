{-# LANGUAGE RecordWildCards #-}

module Visualisation where

import Rendering
import ViewParams
import Graph

data (Graph a) => Visualisation a = Vis {
    title :: String,
    graph :: a,
    viewParams :: ViewParams
}

renderVis :: (Graph a) => Visualisation a -> IO ()
renderVis (Vis title graph viewParams) = do
    renderTitle title
    render graph viewParams