{-# LANGUAGE DeriveGeneric #-}

module RedBanana.Types where

import Data.Aeson
import Data.Text
import Data.Word
import GHC.Generics (Generic)

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

data Block = Block Int [Instr] deriving (Show, Ord, Eq)

data Instr = STOP
           | ADD
           | MUL
           | SUB
           | DIV
           | SDIV
           | MOD
           | SMOD
           | ADDMOD
           | MULMOD
           | EXP
           | SIGNEXTEND
           | LT
           | GT
           | SLT
           | SGT
           | EQ
           | ISZERO
           | AND
           | OR
           | XOR
           | NOT
           | BYTE
           | SHA3
           | ADDRESS
           | BALANCE
           | ORIGIN
           | CALLER
           | CALLVALUE
           | CALLDATALOAD
           | CALLDATASIZE
           | CALLDATACOPY
           | CODESIZE
           | CODECOPY
           | GASPRICE
           | EXTCODESIZE
           | EXTCODECOPY
           | BLOCKHASH
           | COINBASE
           | TIMESTAMP
           | NUMBER
           | DIFFICULTY
           | GASLIMIT
           | POP
           | MLOAD
           | MSTORE
           | MSTORE8
           | SLOAD
           | SSTORE
           | JUMP
           | JUMPI
           | PC
           | MSIZE
           | GAS
           | JUMPDEST
           | PUSH1
           | PUSH2
           | PUSH3
           | PUSH4
           | PUSH5
           | PUSH6
           | PUSH7
           | PUSH8
           | PUSH9
           | PUSH10
           | PUSH11
           | PUSH12
           | PUSH13
           | PUSH14
           | PUSH15
           | PUSH16
           | PUSH17
           | PUSH18
           | PUSH19
           | PUSH20
           | PUSH21
           | PUSH22
           | PUSH23
           | PUSH24
           | PUSH25
           | PUSH26
           | PUSH27
           | PUSH28
           | PUSH29
           | PUSH30
           | PUSH31
           | PUSH32
           | DUP1
           | DUP2
           | DUP3
           | DUP4
           | DUP5
           | DUP6
           | DUP7
           | DUP8
           | DUP9
           | DUP10
           | DUP11
           | DUP12
           | DUP13
           | DUP14
           | DUP15
           | DUP16
           | SWAP1
           | SWAP2
           | SWAP3
           | SWAP4
           | SWAP5
           | SWAP6
           | SWAP7
           | SWAP8
           | SWAP9
           | SWAP10
           | SWAP11
           | SWAP12
           | SWAP13
           | SWAP14
           | SWAP15
           | SWAP16
           | LOG0
           | LOG1
           | LOG2
           | LOG3
           | LOG4
           | CREATE
           | CALL
           | CALLCODE
           | RETURN
           | DELEGATECALL
           | BREAKPOINT
           | RNGSEED
           | SSIZEEXT
           | SLOADBYTES
           | SSTOREBYTES
           | SSIZE
           | STATEROOT
           | TXEXECGAS
           | CALLSTATIC
           | INVALID
           | SUICIDE
           | CONST Word8
           | UNKNOWN Word8
           deriving (Show, Eq, Ord)