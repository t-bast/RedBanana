module RedBanana.Bytecode where

import RedBanana.Types
import Data.Char
import Data.Word
import Data.List
import Prelude hiding (EQ, LT, GT)

getInstr :: Word8 -> Instr
-- 0s: Stop and Arithmetic Operations
getInstr 0x00 = STOP
getInstr 0x01 = ADD
getInstr 0x02 = MUL
getInstr 0x03 = SUB
getInstr 0x04 = DIV
getInstr 0x05 = SDIV
getInstr 0x06 = MOD
getInstr 0x07 = SMOD
getInstr 0x08 = ADDMOD
getInstr 0x09 = MULMOD
getInstr 0x0a = EXP
getInstr 0x0b = SIGNEXTEND
-- 10s: Comparison & Bitwise Logic Operations
getInstr 0x10 = LT
getInstr 0x11 = GT
getInstr 0x12 = SLT
getInstr 0x13 = SGT
getInstr 0x14 = EQ
getInstr 0x15 = ISZERO
getInstr 0x16 = AND
getInstr 0x17 = OR
getInstr 0x18 = XOR
getInstr 0x19 = NOT
getInstr 0x1a = BYTE
-- 20s: SHA3
getInstr 0x20 = SHA3
-- 30s: Environmental Information
getInstr 0x30 = ADDRESS
getInstr 0x31 = BALANCE
getInstr 0x32 = ORIGIN
getInstr 0x33 = CALLER
getInstr 0x34 = CALLVALUE
getInstr 0x35 = CALLDATALOAD
getInstr 0x36 = CALLDATASIZE
getInstr 0x37 = CALLDATACOPY
getInstr 0x38 = CODESIZE
getInstr 0x39 = CODECOPY
getInstr 0x3a = GASPRICE
getInstr 0x3b = EXTCODESIZE
getInstr 0x3c = EXTCODECOPY
-- 40s: Block Information
getInstr 0x40 = BLOCKHASH
getInstr 0x41 = COINBASE
getInstr 0x42 = TIMESTAMP
getInstr 0x43 = NUMBER
getInstr 0x44 = DIFFICULTY
getInstr 0x45 = GASLIMIT
-- 50s: Stack, Memory, Storage and Flow Operations
getInstr 0x50 = POP
getInstr 0x51 = MLOAD
getInstr 0x52 = MSTORE
getInstr 0x53 = MSTORE8
getInstr 0x54 = SLOAD
getInstr 0x55 = SSTORE
getInstr 0x56 = JUMP
getInstr 0x57 = JUMPI
getInstr 0x58 = PC
getInstr 0x59 = MSIZE
getInstr 0x5a = GAS
getInstr 0x5b = JUMPDEST
-- 60s & 70s: Push Operations
getInstr 0x60 = PUSH1
getInstr 0x61 = PUSH2
getInstr 0x62 = PUSH3
getInstr 0x63 = PUSH4
getInstr 0x64 = PUSH5
getInstr 0x65 = PUSH6
getInstr 0x66 = PUSH7
getInstr 0x67 = PUSH8
getInstr 0x68 = PUSH9
getInstr 0x69 = PUSH10
getInstr 0x6a = PUSH11
getInstr 0x6b = PUSH12
getInstr 0x6c = PUSH13
getInstr 0x6d = PUSH14
getInstr 0x6e = PUSH15
getInstr 0x6f = PUSH16
getInstr 0x70 = PUSH17
getInstr 0x71 = PUSH18
getInstr 0x72 = PUSH19
getInstr 0x73 = PUSH20
getInstr 0x74 = PUSH21
getInstr 0x75 = PUSH22
getInstr 0x76 = PUSH23
getInstr 0x77 = PUSH24
getInstr 0x78 = PUSH25
getInstr 0x79 = PUSH26
getInstr 0x7a = PUSH27
getInstr 0x7b = PUSH28
getInstr 0x7c = PUSH29
getInstr 0x7d = PUSH30
getInstr 0x7e = PUSH31
getInstr 0x7f = PUSH32
-- 80s: Duplication Operations
getInstr 0x80 = DUP1
getInstr 0x81 = DUP2
getInstr 0x82 = DUP3
getInstr 0x83 = DUP4
getInstr 0x84 = DUP5
getInstr 0x85 = DUP6
getInstr 0x86 = DUP7
getInstr 0x87 = DUP8
getInstr 0x88 = DUP9
getInstr 0x89 = DUP10
getInstr 0x8a = DUP11
getInstr 0x8b = DUP12
getInstr 0x8c = DUP13
getInstr 0x8d = DUP14
getInstr 0x8e = DUP15
getInstr 0x8f = DUP16
-- 90s: Exchange Operations
getInstr 0x90 = SWAP1
getInstr 0x91 = SWAP2
getInstr 0x92 = SWAP3
getInstr 0x93 = SWAP4
getInstr 0x94 = SWAP5
getInstr 0x95 = SWAP6
getInstr 0x96 = SWAP7
getInstr 0x97 = SWAP8
getInstr 0x98 = SWAP9
getInstr 0x99 = SWAP10
getInstr 0x9a = SWAP11
getInstr 0x9b = SWAP12
getInstr 0x9c = SWAP13
getInstr 0x9d = SWAP14
getInstr 0x9e = SWAP15
getInstr 0x9f = SWAP16
-- a0s: Logging Operations
getInstr 0xa0 = LOG0
getInstr 0xa1 = LOG1
getInstr 0xa2 = LOG2
getInstr 0xa3 = LOG3
getInstr 0xa4 = LOG4
-- f0s: System operations
getInstr 0xf0 = CREATE
getInstr 0xf1 = CALL
getInstr 0xf2 = CALLCODE
getInstr 0xf3 = RETURN
getInstr 0xf4 = DELEGATECALL
getInstr 0xf5 = SUICIDE
getInstr opcode = UNKNOWN opcode

pushSize :: Instr -> Int
pushSize PUSH1  = 1
pushSize PUSH2  = 2
pushSize PUSH3  = 3
pushSize PUSH4  = 4
pushSize PUSH5  = 5
pushSize PUSH6  = 6
pushSize PUSH7  = 7
pushSize PUSH8  = 8
pushSize PUSH9  = 9
pushSize PUSH10 = 10
pushSize PUSH11 = 11
pushSize PUSH12 = 12
pushSize PUSH13 = 13
pushSize PUSH14 = 14
pushSize PUSH15 = 15
pushSize PUSH16 = 16
pushSize PUSH17 = 17
pushSize PUSH18 = 18
pushSize PUSH19 = 19
pushSize PUSH20 = 20
pushSize PUSH21 = 21
pushSize PUSH22 = 22
pushSize PUSH23 = 23
pushSize PUSH24 = 24
pushSize PUSH25 = 25
pushSize PUSH26 = 26
pushSize PUSH27 = 27
pushSize PUSH28 = 28
pushSize PUSH29 = 29
pushSize PUSH30 = 30
pushSize PUSH31 = 31
pushSize PUSH32 = 32
pushSize _ = 0

hexToBytes :: String -> [Word8]
hexToBytes [] = []
hexToBytes (c:cs) | c == '\n' || c == '\r' = hexToBytes cs
hexToBytes (sb:(lb:xs)) = let byte = (fromIntegral $ (digitToInt sb) * 16 + digitToInt lb)
                          in (byte : (hexToBytes xs))

disasm :: [Word8] -> [Instr]
disasm [] = []
disasm (i:is) = instr:rest
    where instr = getInstr i
          rest = let count = pushSize instr in
                    if count > 0 then
                        (map CONST $ take count is) ++ (disasm $ drop count is)
                    else
                        disasm is