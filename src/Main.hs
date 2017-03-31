
module Main where

import           Control.Monad (forM_)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L


import Data.Word (Word8)
import Text.Printf (printf)
import Numeric (readHex)

import AppendixH as Instr


main :: IO ()
main = do
  txt <- L.getContents
  case parseHex txt of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right bytes -> forM_ (parseBytes bytes) $ \case
      Left err -> putStrLn $ "ERROR: " ++ err
      Right instr -> print instr


parseHex :: Text -> Either String [Word8]
parseHex
  = sequence
  . map (\b -> case readHex $ L.unpack b of
      [(val, "")] -> Right val
      _ -> Left $ "Invalid hex: " ++ show b)
  . L.chunksOf 2
  . L.strip


parseBytes :: [Word8] -> [Either String Instruction]
parseBytes = loop . zip [(0::Int)..]
  where
    loop [] = []
    loop ((i, code) : rest)
      = case Instr.fromCode code of
        PUSH n _ -> case splitAt n rest of
          (arg, rest')
            | length arg == n -> Right (PUSH n $ map snd arg) : loop rest'
            | otherwise
              -> [Left $ printf "Not enough bytes for PUSH %i at byte %i" n i]
        instr -> Right instr : loop rest
