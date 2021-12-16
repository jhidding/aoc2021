# Day 16: Packet Decoder

``` {.haskell file=app/Parsing/Binary.hs}
module Parsing.Binary where

import RIO
import qualified Data.Vector as Vector
import Data.Vector (Vector)

data Cursor = Cursor
    { cursorData :: Vector Int8
    , cursorByte :: Int
    , cursorBit  :: Int8
    } deriving (Show)
```
