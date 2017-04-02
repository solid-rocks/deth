
module Main where

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Data.Word (Word8)
import Numeric (readHex, showHex)
import Text.Printf (printf)

import AppendixH as Instr


main :: IO ()
main = do
  txt <- L.getContents
  case parseHex txt of
    Left err -> putStrLn $ "ERROR: " ++ err
    Right bytes -> mapM_ print $ parseBytes bytes


parseHex :: Text -> Either String [Word8]
parseHex
  = sequence
  . map (\b -> case readHex $ L.unpack b of
      [(val, "")] -> Right val
      _ -> Left $ "Invalid hex: " ++ show b)
  . L.chunksOf 2
  . L.strip


data Instr = Instr {
  op  :: InstrDesc,
  arg :: [Word8],
  pos :: Int
}

instance Show Instr where
  show (Instr op arg i) = printf "%04x: %s%s %s"
    i
    (name op) (maybe "" show $ variant op)
    (concatMap (`showHex` "") arg)


parseBytes :: [Word8] -> [Instr]
parseBytes = loop . zip [(0::Int)..]
  where
    loop [] = []
    loop ((i, code) : rest)
      = case Instr.fromCode code of
        op@(InstrDesc {argSize = Just n}) -> case splitAt n rest of
          (arg, rest') -> Instr op (map snd arg) i : loop rest'
        op -> Instr op [] i : loop rest
