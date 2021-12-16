-- ~\~ language=Haskell filename=app/Parsing/Binary.hs
-- ~\~ begin <<lit/day16.md|app/Parsing/Binary.hs>>[0]
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
-- ~\~ end
