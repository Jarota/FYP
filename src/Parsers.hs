{-# LANGUAGE OverloadedStrings #-}

module Parsers (pCSV2, pCSV3) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Types

type Parser = Parsec Void Text

pCSV2 :: Parser [(Float,Float)]
pCSV2 = do
    some pFloats2

pFloats2 :: Parser (Float,Float)
pFloats2 = do
    x <- L.scientific
    void (char ',')
    y <- L.scientific
    void (char '\n')
    let
        x' = toRealFloat x
        y' = toRealFloat y
    return (x', y')

pCSV3 :: Parser [(Float,Float,Float)]
pCSV3 = do
    some pFloats3

pFloats3 :: Parser (Float,Float,Float)
pFloats3 = do
    x <- L.scientific
    void (char ',')
    y <- L.scientific
    void (char ',')
    z <- L.scientific
    void (char '\n')
    let
        x' = toRealFloat x
        y' = toRealFloat y
        z' = toRealFloat z
    return (x', y', z')
