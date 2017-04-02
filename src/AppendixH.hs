
module AppendixH (InstrDesc(..), fromCode) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Word (Word8)
import Text.Printf (printf)



data InstrDesc = InstrDesc
  { name     :: String
  , stackMod :: (Int, Int)
  , variant  :: Maybe Int
  , argSize  :: Maybe Int
  -- TODO: effects
  }


desc :: InstrDesc
desc = InstrDesc undefined (0,0) Nothing Nothing


stackOp :: Int -> Int -> String -> InstrDesc
stackOp inp out nm = desc
  { name = nm
  , stackMod = (inp, out)
  }


codeMap :: IntMap InstrDesc
codeMap = Map.fromList
  $  [(0x00, desc {name = "STOP", stackMod = (0,0)})]
  ++ zip [0x01..]
      (map (stackOp 2 1)
        ["ADD", "MUL", "SUB", "DIV", "SDIV", "MOD", "SMOD"])
  ++ zip [0x08..]
      (map (stackOp 3 1) ["ADDMOD", "MULMOD"])
  ++ zip [0x0a..]
      (map (stackOp 2 1)
        ["EXP", "SIGNEXTEND"])
  ++ zip [0x10..]
      (map (stackOp 2 1) ["LT", "GT", "SLT", "SGT", "EQ"])
  ++ [ (0x15, stackOp 1 1 "ISZERO")
     , (0x16, stackOp 2 1 "AND")
     , (0x17, stackOp 2 1 "OR")
     , (0x18, stackOp 2 1 "XOR")
     , (0x19, stackOp 1 1 "NOT")
     , (0x1a, stackOp 2 1 "BYTE")
     , (0x20, stackOp 2 1 "SHA3")
     , (0x30, stackOp 0 1 "ADDRESS")
     , (0x31, stackOp 1 1 "BALANCE")
     , (0x32, stackOp 0 1 "ORIGIN")
     , (0x33, stackOp 0 1 "CALLER")
     , (0x34, stackOp 0 1 "CALLVALUE")
     , (0x35, stackOp 1 1 "CALLDATALOAD")
     , (0x36, stackOp 0 1 "CALLDATASIZE")
     , (0x37, stackOp 3 0 "CALLDATACOPY")
     , (0x38, stackOp 0 1 "CODESIZE")
     , (0x39, stackOp 3 0 "CODECOPY")
     , (0x3a, stackOp 0 1 "GASPRICE")
     , (0x3b, stackOp 1 1 "EXTCODESIZE")
     , (0x3c, stackOp 4 0 "EXTCODECOPY")
     , (0x40, stackOp 1 1 "BLOCKHASH")
     ]
  ++ zip [0x41..] (map (stackOp 0 1)
    [ "BLOCKHASH", "COINBASE", "TIMESTAMP", "NUMBER"
    , "DIFFICULTY", "GASLIMIT"
    ])
  ++ [ (0x50, stackOp 1 0 "POP")
     , (0x51, stackOp 1 1 "MLOAD")
     , (0x52, stackOp 2 0 "MSTORE")
     , (0x53, stackOp 2 0 "MSTORE8")
     , (0x54, stackOp 1 1 "SLOAD")
     , (0x55, stackOp 2 0 "SSTORE")
     , (0x56, stackOp 1 0 "JUMP")
     , (0x57, stackOp 2 0 "JUMPI")
     , (0x58, stackOp 0 1 "PC")
     , (0x59, stackOp 0 1 "MSIZE")
     , (0x5a, stackOp 0 1 "GAS")
     , (0x5b, stackOp 0 0 "JUMPDEST")
     ]
  ++ [ (0x60+i-1, desc') | i <- [1..32]
     , let desc' = desc
            { name = "PUSH"
            , stackMod = (0,1)
            , variant = Just i
            , argSize = Just i}
     ]
  ++ [ (0x80+i-1, desc') | i <- [1..16]
     , let desc' = desc
            { name = "DUP"
            , stackMod = (i, i+1)
            , variant = Just i}
     ]
  ++ [ (0x90+i-1, desc') | i <- [1..16]
     , let desc' = desc
            { name = "SWAP"
            , stackMod = (i+1, i+1)
            , variant = Just i}
     ]
  ++ [ (0xa0+i, desc') | i <- [0..4]
     , let desc' = desc
            { name = "LOG"
            , stackMod = (i+2, 0)
            , variant = Just i}
     ]
  ++ [ (0xf0, stackOp 3 1 "CREATE")
     , (0xf1, stackOp 7 1 "CALL")
     , (0xf2, stackOp 7 1 "CALLCODE")
     , (0xf3, stackOp 2 0 "RETURN")
     , (0xf4, stackOp 6 1 "DELEGATECALL")
     , (0xff, stackOp 1 0 "SUICIDE")
     ]


fromCode :: Word8 -> InstrDesc
fromCode c
  = maybe (desc {name = printf "INVALID 0x%02x" c}) id
  $ Map.lookup (fromIntegral c) codeMap
