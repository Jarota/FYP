{-# LANGUAGE OverloadedStrings #-}

module Parsers where

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

pCSV :: Parser [Float]
pCSV = do
    some pFloat

pFloat :: Parser Float
pFloat = do
	x <- L.scientific
	void separator
	return (toRealFloat x)

separator :: Parser Char
separator = char ','
	<|>		char '\n'
