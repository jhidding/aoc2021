-- ~\~ language=Haskell filename=app/Parsing/Binary.hs
-- ~\~ begin <<lit/day16.md|app/Parsing/Binary.hs>>[0]
module Parsing.Binary where

import RIO
import qualified Data.Vector as Vector
import Data.Vector (Vector)

data Cursor = Cursor
    { cursorData :: Vector Int8
    , cursorByte :: Int
    , cursorBit  :: Int8
    } deriving (Show)
-- ~\~ end
