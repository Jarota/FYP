{-# LANGUAGE OverloadedStrings #-}

module PieParser (pPie) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Graphics.UI.GLUT

import Graph
import DataSet
import HelperParsers

pPie :: Parser Graph
pPie = do
    d <- pDataSet
    ds <- many pDataSets
    return $ Pie (d:ds)

pDataSets :: Parser DataSet
pDataSets = do
    void newline
    d <- pDataSet
    return d

pDataSet :: Parser DataSet
pDataSet = do
    void $ string "\nDataset:"
    label <- pNewlineSeparatedString
    dataColor <- pColor
    void newline
    pDataSet' label dataColor

pDataSet' :: String -> Color4 GLfloat -> Parser DataSet
pDataSet' label dataColor = do
        void $ string "File:"
        file <- pFileString
        return $ File label dataColor file
    <|> do
        void $ string "n:"
        n <- pFloatItem
        return $ Raw label dataColor [(toGraphData [n])]