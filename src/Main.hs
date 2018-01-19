
module Main where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy as LB

import Numeric (readHex)
import EVM.Instructions (readProgram)


main :: IO ()
main = do
  txt <- LT.getContents
  case parseHex txt of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right bytes -> mapM_ print $ readProgram bytes 0


parseHex :: Text -> Either String LB.ByteString
parseHex
  = fmap LB.pack
  . sequence
  . map (\b -> case readHex $ LT.unpack b of
      [(val, "")] -> Right val
      _ -> Left $ "Invalid hex: " ++ show b)
  . LT.chunksOf 2
  . LT.strip
