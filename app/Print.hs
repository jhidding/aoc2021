-- ~\~ language=Haskell filename=app/Print.hs
-- ~\~ begin <<lit/boilerplate.md|app/Print.hs>>[0]
module Print where

import RIO
import qualified RIO.Text as Text
import RIO.ByteString (putStr)
import Data.Massiv.Array (Ix2(..))

print :: (MonadIO m) => Text -> m ()
print = putStr . Text.encodeUtf8

printLn :: (MonadIO m) => Text -> m ()
printLn = print . (<> "\n") 

printCoords :: MonadIO m => [Ix2] -> m ()
printCoords = mapM_ (\(x :. y) -> printLn $ tshow x <> " " <> tshow y)
-- ~\~ end
