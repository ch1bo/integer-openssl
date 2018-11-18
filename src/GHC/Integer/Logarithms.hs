{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Integer.Logarithms where

import GHC.Prim
import GHC.Integer.Type

-- TODO(SN): implement
integerLogBase# :: Integer -> Integer -> Int#
integerLogBase# _ _ = 0#

-- TODO(SN): implement
integerLog2# :: Integer -> Int#
integerLog2# _ = 0#

-- TODO(SN): implement
wordLog2# :: Word# -> Int#
wordLog2# _ = 0#
