{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Integer.Logarithms.Internals where

import GHC.Prim
import GHC.Integer.Type

-- TODO(SN): implement
integerLog2# :: Integer -> Int#
integerLog2# _ = 0#

-- TODO(SN): implement
wordLog2# :: Word# -> Int#
wordLog2# _ = 0#

-- TODO(SN) implement
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
integerLog2IsPowerOf2# _ = (# 0#, 0# #)

-- TODO(SN) implement
roundingMode# :: Integer -> Int# -> Int#
roundingMode# _ _ = 0#
