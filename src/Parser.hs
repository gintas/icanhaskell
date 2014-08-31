module Parser where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Error


data FunDecl = FunDecl { funName :: String, funType :: String } deriving (Eq, Show)

data TypeExpr = TypeId String
              | TypeVar String
              | TypeImpl TypeExpr TypeExpr
 deriving (Eq, Show)

typeParser :: GenParser Char st TypeExpr
typeParser =
  try parseImpl <|> parseTypeToken
  where parseTypeId = do
          c <- upper
          rest <- many alphaNum 
          return (TypeId (c:rest))
        parseTypeVar = do
          c <- lower
          rest <- many alphaNum 
          return (TypeVar (c:rest))
        parseTypeToken = parseTypeId <|> parseTypeVar
        parseImpl = do
          tok1 <- parseTypeToken
          spaces
          _ <- string "->"
          spaces
          tok2 <- typeParser
          return (TypeImpl tok1 tok2)
   
declParser :: GenParser Char st FunDecl
declParser = do
  name <- many alphaNum
  spaces
  _ <- string "::"
  spaces
  typeDecl <- many anyChar
  return (FunDecl name typeDecl)


parseInput :: String -> Either ParseError FunDecl
parseInput input = parse declParser "(error)" input

parseTypeExpr :: String -> Either ParseError TypeExpr
parseTypeExpr input = parse typeParser "(error)" input 

p = parseTypeExpr