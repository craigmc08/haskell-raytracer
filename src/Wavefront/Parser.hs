module Wavefront.Parser where

import Text.ParserCombinators.Parsec
import Wavefront.Types
import Control.Applicative (liftA2)

line :: GenParser Char st WavefrontLine
line = choice [comment, vert, face, ignore]

comment :: GenParser Char st WavefrontLine
comment = char '#' *> ignore

ignore :: GenParser Char st WavefrontLine
ignore = do
  cmmnt <- anyChar `manyTill` lookAhead (char '\n')
  return $ Comment cmmnt

vert :: GenParser Char st WavefrontLine
vert = do
  char 'v'
  choice [vertex, texcoord, normal]

vertex :: GenParser Char st WavefrontLine
vertex = do
  char ' '
  x <- double
  char ' '
  y <- double
  char ' '
  z <- double
  return $ Vertex x y z

texcoord :: GenParser Char st WavefrontLine
texcoord = do
  char 't'
  char ' '
  u <- double
  char ' '
  v <- double
  return $ TexCoord u v

normal :: GenParser Char st WavefrontLine
normal = do
  char 'n'
  char ' '
  x <- double
  char ' '
  y <- double
  char ' '
  z <- double
  return $ Normal x y z

face :: GenParser Char st WavefrontLine
face = do
  char 'f'
  char ' '
  i0 <- integer
  char '/'
  i1 <- integer
  char '/'
  i2 <- integer
  char ' '
  j0 <- integer
  char '/'
  j1 <- integer
  char '/'
  j2 <- integer
  char ' '
  k0 <- integer
  char '/'
  k1 <- integer
  char '/'
  k2 <- integer
  return $ Face (i0 - 1) (i1 - 1) (i2 - 1) (j0 - 1) (j1 - 1) (j2 - 1) (k0 - 1) (k1 - 1) (k2 - 1)

-- Number parsing with inspiration from
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

integer :: GenParser Char st Int
integer = fmap read integral

number :: GenParser Char st String
number = many1 digit

plus :: GenParser Char st String
plus = char '+' *> number

minus :: GenParser Char st String
minus = char '-' <:> number

integral :: GenParser Char st String
integral = plus <|> minus <|> number

double :: GenParser Char st Double
double = fmap read $ integral <++> decimal <++> exponent
  where decimal = option "" $ char '.' <:> number
        exponent = option "" $ oneOf "eE" <:> integral

obj :: GenParser Char st [WavefrontLine]
obj = line `endBy` (char '\n')

parseObj :: String -> String -> Either ParseError [WavefrontLine]
parseObj = parse obj
