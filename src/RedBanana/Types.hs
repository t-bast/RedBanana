{-# LANGUAGE DeriveGeneric #-}

module RedBanana.Types where

import Data.Aeson
import Data.Text
import Data.Word
import GHC.Generics (Generic)
import Compiler.Hoopl

data Transaction = Transaction 
    { hash :: Text
    , blockHash :: Text
    , blockNumber :: Integer
    , confirmations :: Integer
    , from :: Text
    , to :: Text
    , value :: Integer } 
    deriving (Generic, Show)

instance FromJSON Transaction

data TransactionArgs = TransactionArgs
    { address :: Text
    , startBlock :: Int
    , endBlock :: Int
    , sort :: Text }
    deriving (Show)

data Instr e x where
    STOP :: Instr O C
    ADD :: Instr O O
    MUL :: Instr O O
    SUB :: Instr O O
    DIV :: Instr O O
    SDIV :: Instr O O
    MOD :: Instr O O
    SMOD :: Instr O O
    ADDMOD :: Instr O O
    MULMOD :: Instr O O
    EXP :: Instr O O
    LT :: Instr O O
    GT :: Instr O O
    SLT :: Instr O O
    SGT :: Instr O O
    EQ :: Instr O O
    ISZERO :: Instr O O
    AND :: Instr O O
    OR :: Instr O O
    XOR :: Instr O O
    NOT :: Instr O O
    BYTE :: Instr O O
    SHA3 :: Instr O O
    ADDRESS :: Instr O O
    BALANCE :: Instr O O
    ORIGIN :: Instr O O
    CALLER :: Instr O O
    CALLVALUE :: Instr O O
    CALLDATALOAD :: Instr O O
    CALLDATASIZE :: Instr O O
    CALLDATACOPY :: Instr O O
    CODESIZE :: Instr O O
    CODECOPY :: Instr O O
    GASPRICE :: Instr O O
    EXTCODESIZE :: Instr O O
    EXTCODECOPY :: Instr O O
    BLOCKHASH :: Instr O O
    COINBASE :: Instr O O
    TIMESTAMP :: Instr O O
    NUMBER :: Instr O O
    DIFFICULTY :: Instr O O
    GASLIMIT :: Instr O O
    POP :: Instr O O
    MLOAD :: Instr O O
    MSTORE :: Instr O O
    MSTORE8 :: Instr O O
    SLOAD :: Instr O O
    SSTORE :: Instr O O
    JUMP :: Instr O C
    JUMPI :: Instr O C 
    PC :: Instr O O
    MSIZE :: Instr O O
    GAS :: Instr O O
    JUMPDEST :: Instr C O
    PUSH1 :: Instr O O
    PUSH2 :: Instr O O
    PUSH3 :: Instr O O
    PUSH4 :: Instr O O
    PUSH5 :: Instr O O
    PUSH6 :: Instr O O
    PUSH7 :: Instr O O
    PUSH8 :: Instr O O
    PUSH9 :: Instr O O
    PUSH10 :: Instr O O
    PUSH11 :: Instr O O
    PUSH12 :: Instr O O
    PUSH13 :: Instr O O
    PUSH14 :: Instr O O
    PUSH15 :: Instr O O
    PUSH16 :: Instr O O
    PUSH17 :: Instr O O
    PUSH18 :: Instr O O
    PUSH19 :: Instr O O
    PUSH20 :: Instr O O
    PUSH21 :: Instr O O
    PUSH22 :: Instr O O
    PUSH23 :: Instr O O
    PUSH24 :: Instr O O
    PUSH25 :: Instr O O
    PUSH26 :: Instr O O
    PUSH27 :: Instr O O
    PUSH28 :: Instr O O
    PUSH29 :: Instr O O
    PUSH30 :: Instr O O
    PUSH31 :: Instr O O
    PUSH32 :: Instr O O
    DUP1 :: Instr O O
    DUP2 :: Instr O O
    DUP3 :: Instr O O
    DUP4 :: Instr O O
    DUP5 :: Instr O O
    DUP6 :: Instr O O
    DUP7 :: Instr O O
    DUP8 :: Instr O O
    DUP9 :: Instr O O
    DUP10 :: Instr O O
    DUP11 :: Instr O O
    DUP12 :: Instr O O
    DUP13 :: Instr O O
    DUP14 :: Instr O O
    DUP15 :: Instr O O
    DUP16 :: Instr O O
    SWAP1 :: Instr O O
    SWAP2 :: Instr O O
    SWAP3 :: Instr O O
    SWAP4 :: Instr O O
    SWAP5 :: Instr O O
    SWAP6 :: Instr O O
    SWAP7 :: Instr O O
    SWAP8 :: Instr O O
    SWAP9 :: Instr O O
    SWAP10 :: Instr O O
    SWAP11 :: Instr O O
    SWAP12 :: Instr O O
    SWAP13 :: Instr O O
    SWAP14 :: Instr O O
    SWAP15 :: Instr O O
    SWAP16 :: Instr O O
    LOG0 :: Instr O O
    LOG1 :: Instr O O
    LOG2 :: Instr O O
    LOG3 :: Instr O O
    LOG4 :: Instr O O
    CREATE :: Instr O O
    CALL :: Instr O C
    CALLCODE :: Instr O C
    RETURN :: Instr O C
    DELEGATECALL :: Instr O C
    BREAKPOINT :: Instr O O
    RNGSEED :: Instr O O
    SSIZEEXT :: Instr O O
    SLOADBYTES :: Instr O O
    SSTOREBYTES :: Instr O O
    SSIZE :: Instr O O
    STATEROOT :: Instr O O
    TXEXECGAS :: Instr O O
    CALLSTATIC :: Instr O C
    INVALID :: Instr O C
    SUICIDE :: Instr O C
    CONST :: Word8 -> Instr O O
    UNKNOWN :: Word8 -> Instr O O