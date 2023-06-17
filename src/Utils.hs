module Utils where

import Data.ByteString.Char8 qualified as BS
import Data.Kind
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.IO (stdout)

iforM
  :: forall (m :: Type -> Type) (a :: Type) (b :: Type)
   . Monad m
  => [a]
  -> (Int -> a -> m b)
  -> m [b]
iforM list fun = go ilist
  where
    ilist = zip [0 ..] list
    go l = mapM (uncurry fun) l

say :: String -> IO ()
say = BS.hPutStrLn stdout . Text.encodeUtf8 . Text.pack
