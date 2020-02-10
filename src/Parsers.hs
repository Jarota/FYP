{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import Control.Applicative
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

p :: Parser Scientific
p = L.scientific
