-- ~\~ language=Haskell filename=app/Day16.hs
-- ~\~ begin <<lit/day16.md|app/Day16.hs>>[0]
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

-- ~\~ begin <<lit/day16.md|parser-day16>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day16.md|parser-day16>>[1]
readBitstream :: (MonadReader env m, MonadIO m, HasLogFunc env)
              => Bitstream Right -> BitParser a -> m a
readBitstream b p =
    either (\e -> do { logError $ display (tshow e); exitFailure })
           return (parse p "-" b)

readInput :: (HasLogFunc env) => RIO env Packet
readInput = do
    bits <- readInputParsing "data/day16.txt" bitstream
    readBitstream bits packet
-- ~\~ end

-- ~\~ begin <<lit/day16.md|bits-parser-day16>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day16.md|evaluator-day16>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day16.md|solution-day16>>[0]
getVersions :: Packet -> [Int]
getVersions Packet {..} = [packetVersion] <> contentVersions packetContent
    where contentVersions (OperatorPacket _ p) = concatMap getVersions p
          contentVersions _                  = []

solutionA :: Packet -> Int
solutionA = sum . getVersions

solutionB :: Packet -> Int
solutionB = evalPacket

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
-- ~\~ end
