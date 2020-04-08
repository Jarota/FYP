{-# LANGUAGE OverloadedStrings #-}

module Parsers (pVisualisation) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import HelperParsers
import Bar2DParser
import Scatter2DParser
import Scatter3DParser

import Visualisation
import ViewParams
import Graph

-- Enum type for branching to correct graph parser
data GraphType = Sca2D | Sca3D | Ba2D


{- DSL Parsers -}

pVisualisation :: Parser Visualisation
pVisualisation = do
    title <- pNewlineSeparatedString
    graphType <- pGraphType
    void newline
    graph <- pGraph graphType
    return (Vis title graph initViewParams)

pGraphType :: Parser GraphType
pGraphType = choice [
    Sca2D   <$ string "Scatter2D",
    Sca3D   <$ string "Scatter3D",
    Ba2D    <$ string "Bar2D"
    -- Pi      <$ string "Pie"
    ]

pGraph :: GraphType -> Parser Graph
pGraph graphType = case graphType of
    Sca2D   -> pScatter2D
    Sca3D   -> pScatter3D
    Ba2D    -> pBar2D
    -- Pi      -> pPie
