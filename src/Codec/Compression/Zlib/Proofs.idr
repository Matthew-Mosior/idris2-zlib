module Codec.Compression.Zlib.Proofs

import Data.Array.Index

%default total

--------------------------------------------------------------------------------
--          Proofs
--------------------------------------------------------------------------------

||| Every natural number is less than or equal to itself.
|||
export
lteReflexive : (n : Nat) -> LTE n n
lteReflexive 0     = LTEZero
lteReflexive (S n) = LTESucc (lteReflexive n)

||| Proof that a full buffer range beginning at offset zero is in bounds.
|||
export
zeroOffsetInBounds : (n : Nat) -> LTE (0 + n) n
zeroOffsetInBounds n =
  lteReflexive n

||| Zero is less than or equal to every natural number.
|||
export
zeroLTE : (n : Nat) -> LTE 0 n
zeroLTE _ = LTEZero
