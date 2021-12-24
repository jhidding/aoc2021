# Day 24: Arithmetic Logic Unit

``` {.haskell file=app/Day24.hs}
module Day24 where

import RIO
import Parsing (Parser, string, char, integer, readInputParsing, sepEndBy1, eol, lexeme)
import Text.Megaparsec.Char (lowerChar)
import Tally (Tally(..))

data Var = W | X | Y | Z deriving (Show, Eq, Ord, Enum)
data Val = Ref Var | Lit Int deriving (Show, Eq, Ord)
data Instr
    = Inp Var
    | Add Var Val
    | Mul Var Val
    | Div Var Val
    | Mod Var Val
    | Eql Var Val
    deriving (Show, Eq, Ord)


type Memory n = Maybe (n, n, n, n)

var W = _1
var X = _2
var Y = _3
var Z = _4

read :: (MonadState (Memory n) m) => Var -> m n
read a = use (var a)

write :: (MonadState (Memory n) m) => Var -> n -> m ()
write a = ((var a) .=)

modify :: (MonadState (Memory n) m) => Var -> (n -> n) -> m ()
modify a = ((var a) %=)

resolve :: (MonadState (Memory n) m, Num n) => Val -> m n
resolve (Var a) = read a
resolve (Lit a) = fromInteger a

varP :: Parser Var
varP = lexeme (  (char 'w' $> W)
             <|> (char 'x' $> X)
             <|> (char 'y' $> Y)
             <|> (char 'z' $> Z))

valP :: Parser Val
valP = lexeme $ (Ref <$> varP) <|> (Lit <$> integer)

instrP :: Parser Instr
instrP =  (lexeme (string "inp") $> Inp <*> varP)
      <|> (lexeme (string "add") $> Add <*> varP <*> valP)
      <|> (lexeme (string "mul") $> Mul <*> varP <*> valP)
      <|> (lexeme (string "div") $> Div <*> varP <*> valP)
      <|> (lexeme (string "mod") $> Mod <*> varP <*> valP)
      <|> (lexeme (string "eql") $> Eql <*> varP <*> valP)

readInput :: (HasLogFunc env) => RIO env [Instr]
readInput = readInputParsing "data/day24.txt" (instrP `sepEndBy1` eol)


eval :: (MonadState Memory m, MonadReader n m, Num n) => Instr -> m ()
eval (Inp a) = ask >>= write a
eval (Add a b) = resolve b  modify a

solutionA = id
solutionB = const 0

<<run-solutions>>
```
