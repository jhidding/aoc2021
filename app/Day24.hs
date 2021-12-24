-- ~\~ language=Haskell filename=app/Day24.hs
-- ~\~ begin <<lit/day24.md|app/Day24.hs>>[0]
module Day24 where

import RIO
import Parsing (Parser, string, char, integer, readInputParsing, sepEndBy1, eol, lexeme)
import Text.Megaparsec.Char (lowerChar)

data Var = W | X | Y | Z deriving (Show, Eq, Ord)
data Val = Ref Var | Lit Int deriving (Show, Eq, Ord)
data Instr
    = Inp Var
    | Add Val Val
    | Mul Val Val
    | Div Val Val
    | Mod Val Val
    | Eql Val Val
    deriving (Show, Eq, Ord)

varP :: Parser Var
varP = lexeme (  (char 'w' $> W)
             <|> (char 'x' $> X)
             <|> (char 'y' $> Y)
             <|> (char 'z' $> Z))

valP :: Parser Val
valP = lexeme $ (Ref <$> varP) <|> (Lit <$> integer)

instrP :: Parser Instr
instrP =  (lexeme (string "inp") $> Inp <*> varP)
      <|> (lexeme (string "add") $> Add <*> valP <*> valP)
      <|> (lexeme (string "mul") $> Mul <*> valP <*> valP)
      <|> (lexeme (string "div") $> Div <*> valP <*> valP)
      <|> (lexeme (string "mod") $> Mod <*> valP <*> valP)
      <|> (lexeme (string "eql") $> Eql <*> valP <*> valP)

readInput :: (HasLogFunc env) => RIO env [Instr]
readInput = readInputParsing "data/day24.txt" (instrP `sepEndBy1` eol)

solutionA = id
solutionB = const 0

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
