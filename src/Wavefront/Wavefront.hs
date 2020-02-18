module Wavefront.Wavefront where

import Text.ParserCombinators.Parsec
import Wavefront.Parser
import Wavefront.Converter
import Types

readObj :: String -> String -> Either ParseError [Tri]
readObj source text = fmap obj2Mesh $ parseObj source text
