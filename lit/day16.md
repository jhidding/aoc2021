# Day 16: Packet Decoder
Oh boy. I defined a `Stream` instance for Megaparsec on the `Bitstream` type from the `bitstreams` package. This lets me define some elementary parsers.

``` {.haskell file=app/Parsing/Binary.hs}
{-# LANGUAGE UndecidableInstances #-}
module Parsing.Binary where

import RIO
import Data.Bits (Bits)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Bitstream as BS
import Data.Bitstream.Packet (toOctet)
import qualified Data.Bitstream.Generic as BSG
import Text.Megaparsec (Parsec, Stream(..), takeP, anySingle)

instance (BSG.Bitstream (BS.Bitstream d)) => Stream (BS.Bitstream d) where
    type Token (BS.Bitstream d) = Bool
    type Tokens (BS.Bitstream d) = BS.Bitstream d

    tokensToChunk pxy = BS.pack
    chunkToTokens pxy = BS.unpack
    chunkLength pxy = BS.length
    chunkEmpty pxy = BS.null
    take1_ s
        | BS.null s = Nothing
        | otherwise = Just (BS.head s, BS.tail s)
    takeN_ n s
        | BS.length s < n = Nothing
        | otherwise = Just (BS.take n s, BS.drop n s)
    takeWhile_ = BS.span

type BitParser = Parsec Void (BS.Bitstream BS.Right)

intN :: (Integral n, Bits n) => Int -> BitParser n
intN n = BS.toBits <$> takeP (Just $ show n <> " bit integer") n

bool :: BitParser Bool
bool = anySingle

bit :: BitParser Word8
bit = intN 1

skip :: Int -> BitParser ()
skip n = void $ takeP (Just $ "skipping " <> show n <> " bits") n
```

Now, I have a lot of imports.

``` {.haskell file=app/Day16.hs}
module Day16 where

import RIO hiding (bool, try)
import RIO.Char (ord)
import RIO.List.Partial (foldl1')
import RIO.Partial (toEnum)
import qualified RIO.Map as Map
import qualified RIO.ByteString as ByteString
import Data.Bitstream (Bitstream, Right)
import qualified Data.Bitstream as Bitstream
import Parsing (Parser, readInputParsing)
import Parsing.Binary (BitParser, intN, bool, skip, bit)
import Control.Monad (replicateM)
import Text.Megaparsec (parse, chunk, try, takeP)
import Text.Megaparsec.Char (hexDigitChar)

<<parser-day16>>
<<data-type-day16>>
<<bits-parser-day16>>
<<evaluator-day16>>
<<solution-day16>>
```

First we need to parse the hexadecimal notation to a `Bitstream Right` object.

``` {.haskell #parser-day16}
nibble :: Parser Word8
nibble = hexDigitChar >>= toValue 
    where toValue c
            | '0' <= c && c <= '9' = return $ fromIntegral $ ord c - ord '0'
            | 'a' <= c && c <= 'f' = return $ fromIntegral $ ord c - ord 'a' + 10
            | 'A' <= c && c <= 'F' = return $ fromIntegral $ ord c - ord 'A' + 10
            | otherwise = fail "not a hexadecimal character"

byte :: Parser Word8
byte = combine <$> nibble <*> nibble
    where combine a b = a*16 + b

bitstream :: Parser (Bitstream Right)
bitstream = Bitstream.fromByteString . ByteString.pack <$> some byte
```

Then I parse directly to a single packet:

``` {.haskell #parser-day16}
readBitstream :: (MonadReader env m, MonadIO m, HasLogFunc env)
              => Bitstream Right -> BitParser a -> m a
readBitstream b p =
    either (\e -> do { logError $ display (tshow e); exitFailure })
           return (parse p "-" b)

readInput :: (HasLogFunc env) => RIO env Packet
readInput = do
    bits <- readInputParsing "data/day16.txt" bitstream
    readBitstream bits packet
```

These are my data types: a `TypeId`, a `Packet` container and `PacketContent` which is either a literal value or an operator.

``` {.haskell #data-types-day16}
data TypeId
    = SumId
    | ProductId
    | MinimumId
    | MaximumId
    | LiteralValueId
    | GreaterThanId
    | LessThanId
    | EqualToId
    deriving (Show, Eq, Ord, Enum)

data Packet = Packet
    { packetVersion :: Int
    , packetContent :: PacketContent
    } deriving (Show)

data PacketContent
    = LiteralValuePacket Int
    | OperatorPacket TypeId [Packet]
    deriving (Show)
```

So, now we need to parse the bit stream to a `Packet` object. These are a quite direct translation of the problem text into code. The ugly bit is that inside `operatorPacket`, we need to call the `parse` function recursively.

``` {.haskell #bits-parser-day16}
version :: BitParser Int
version = intN 3

typeId :: TypeId -> BitParser ()
typeId i = void $ chunk (Bitstream.fromNBits @Word8 @Int 3 (fromEnum i))

literalValuePacket :: BitParser PacketContent
literalValuePacket = do
    typeId LiteralValueId
    loop 0
    where loop n = do
            continue <- bool
            nib <- intN @Word8 4
            let n' = n * 16 + fromIntegral nib
            if continue then loop n'
            else return $ LiteralValuePacket n'

operatorPacket :: BitParser PacketContent
operatorPacket = do
    typeId <- toEnum <$> intN @Int 3
    lengthType <- bit
    if lengthType == 0 then do
        l <- intN 15
        subbits <- takeP (Just "sub-packets") l
        subpkts <- either (fail . show) return $ parse (some packet) "-" subbits
        return $ OperatorPacket typeId subpkts
    else do
        l <- intN 11
        OperatorPacket typeId <$> replicateM l packet

packet :: BitParser Packet
packet = do
    packetVersion <- version
    packetContent <- try literalValuePacket
                 <|> operatorPacket
    return $ Packet {..}
```

To solve part A, we need to sum all version numbers.

``` {.haskell #solution-day16}
getVersions :: Packet -> [Int]
getVersions Packet {..} = [packetVersion] <> contentVersions packetContent
    where contentVersions (OperatorPacket _ p) = concatMap getVersions p
          contentVersions _                  = []

solutionA :: Packet -> Int
solutionA = sum . getVersions

solutionB :: Packet -> Int
solutionB = evalPacket

<<run-solutions>>
```

For part B, we need to evaluate the computation that is contained in the message.

``` {.haskell #evaluator-day16}
evalPacket :: Packet -> Int
evalPacket Packet{..} = eval packetContent
    where eval (LiteralValuePacket i) = i
          eval (OperatorPacket op p) = eval' op p
          eval' SumId p     = sum (map evalPacket p)
          eval' ProductId p = product (map evalPacket p)
          eval' MinimumId p = foldl1' min (map evalPacket p)
          eval' MaximumId p = foldl1' max (map evalPacket p)
          eval' GreaterThanId [a, b] = if evalPacket a > evalPacket b then 1 else 0
          eval' LessThanId [a, b] = if evalPacket a < evalPacket b then 1 else 0
          eval' EqualToId [a, b] = if evalPacket a == evalPacket b then 1 else 0
```

So this code is still full of partial functions, which is not so nice, but it's getting late.