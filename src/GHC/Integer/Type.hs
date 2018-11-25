{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
module GHC.Integer.Type where

#include "MachDeps.h"

import GHC.Classes
import GHC.Magic
import GHC.Prim
import GHC.Types

-- * Integer functions

-- TODO(SN): general:
--  - add short cuts
--  - look into lazyness (bang patterns)
--  - consistent style: 'monadic' vs explict state#
--  - 32-bit support
--  - additional Integer operations: gcdInteger, lcmInteger, bitInteger


#if WORD_SIZE_IN_BITS == 64
# define INT_MINBOUND      -0x8000000000000000
# define INT_MAXBOUND       0x7fffffffffffffff
# define ABS_INT_MINBOUND   0x8000000000000000
# define WORD_SIZE_IN_BYTES 8
# define WORD_SHIFT         3
# define HIGH_HALF_SHIFT    32
# define LOW_HALF_MASK      0xffffffff
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
# define HIGH_HALF_SHIFT    16
# define LOW_HALF_MASK      0xffff
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

data Integer = S# !Int#
               -- ^ small integer
             | Bp# {-# UNPACK #-} !BigNum
               -- ^ positive bignum, > maxbound(Int)
             | Bn# {-# UNPACK #-} !BigNum
               -- ^ negative bignum, < minbound(Int)

instance Eq Integer  where
    a == b = isTrue# (eqInteger# a b)
    a /= b = isTrue# (neqInteger# a b)

instance Ord Integer where
    a <= b = isTrue# (leInteger# a b)
    a >  b = isTrue# (gtInteger# a b)
    a <  b = isTrue# (ltInteger# a b)
    a >= b = isTrue# (geInteger# a b)
    compare = compareInteger

-- ** Creation and conversion

-- | Construct 'Integer' value from list of 'Int's.
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least
                    -- significant first
          -> Integer
mkInteger nonNegative is
  | nonNegative = f is
  | True = negateInteger (f is)
 where
  f [] = S# 0#
  f (I# i : is') = smallInteger (i `andI#` 0x7fffffff#) `orInteger` shiftLInteger (f is') 31#
{-# NOINLINE mkInteger #-}

-- | Create an Integer from a single Int#.
smallInteger :: Int# -> Integer
smallInteger i# = S# i#
{-# NOINLINE smallInteger #-}

-- | Create a (positive) Integer from a single Word#.
wordToInteger :: Word# -> Integer
wordToInteger w#
  | isTrue# (i# >=# 0#) = S# i#
  | True = Bp# (wordToBigNum w#)
  where
    i# = word2Int# w#
{-# NOINLINE wordToInteger #-}

-- | Create a (negative) Integer from a single Word#.
wordToNegInteger :: Word# -> Integer
wordToNegInteger w#
  | isTrue# (i# <=# 0#) = S# i#
  | True = Bn# (wordToBigNum w#)
  where
    i# = negateInt# (word2Int# w#)
-- inlinable as only internally used

-- | Truncates to least significant
integerToWord :: Integer -> Word#
integerToWord (S# i#) = int2Word# i#
integerToWord (Bp# bn) = bigNumToWord bn
integerToWord (Bn# bn) = int2Word# (negateInt# (bigNumToInt bn))
{-# NOINLINE integerToWord #-}

integerToInt :: Integer -> Int#
integerToInt (S# i#) = i#
integerToInt (Bp# bn) = bigNumToInt bn
integerToInt (Bn# bn) = negateInt# (bigNumToInt bn)
{-# NOINLINE integerToInt #-}

floatFromInteger :: Integer -> Float#
floatFromInteger i = double2Float# (doubleFromInteger i)
{-# NOINLINE floatFromInteger #-}

doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (Bp# bn) = doubleFromPositive bn
doubleFromInteger (Bn# bn) = negateDouble# (doubleFromPositive bn)
{-# NOINLINE doubleFromInteger #-}

-- | helper function to convert a positive Integer into a Double#
doubleFromPositive :: BigNum -> Double#
doubleFromPositive bn = doubleFromPositive' bn 0#
  where
    doubleFromPositive' bn !idx =
      let n = wordsInBigNum# bn
          newIdx = idx +# 1# in
      case isTrue# (idx ==# n) of
        True ->  0.0##
        _ -> case bigNumIdx bn idx of
                x -> case splitHalves x of
                  (# h, l #) -> (doubleFromPositive' bn newIdx)
                      *## (2.0## **## WORD_SIZE_IN_BITS_FLOAT## )
                      +## int2Double# (word2Int# h) *## (2.0## **## int2Double# HIGH_HALF_SHIFT#)
                      +## int2Double# (word2Int# l)

-- | splits the given Word# into a high- and a low-word
splitHalves :: Word# -> (# {- High -} Word#, {- Low -} Word# #)
splitHalves (!x) = (# x `uncheckedShiftRL#` HIGH_HALF_SHIFT#,
                      x `and#` LOW_HALF_MASK## #)

-- | encodes the given integer into a double with the given exponent.
-- | encodeDoubleInteger i e = i * 2 ^ e
{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# INT_MINBOUND#) 0# = negateDouble# (encodeDouble# (int2Word# INT_MINBOUND#) 0#)
encodeDoubleInteger (S# INT_MINBOUND#) e0 = encodeDouble# (int2Word# INT_MINBOUND#) e0
encodeDoubleInteger (S# i) e0
  | isTrue# (i >=# 0#) = encodeDouble# (int2Word# i) e0
  | True = negateDouble# (encodeDouble# (int2Word# (negateInt# i)) e0)
encodeDoubleInteger (Bp# bn) e0 = f 0.0## 0# e0
    where
      f !acc !idx !e =
        let n = wordsInBigNum# bn
            newIdx = idx +# 1# in
        case isTrue# (idx ==# n) of
          True -> acc
          _ ->
              let d = bigNumIdx bn idx
                  newAcc = acc +## encodeDouble# d e
                  newE = e +# WORD_SIZE_IN_BITS#
              in
              f newAcc newIdx newE
encodeDoubleInteger (Bn# bn) e0 =
  negateDouble# (encodeDoubleInteger (Bp# bn) e0)

-- | __word_encodeDouble does simply do some preparations and then
-- calls 'ldexp p1 p2' in C
foreign import ccall unsafe "__word_encodeDouble"
        encodeDouble# :: Word# -> Int# -> Double#

-- | Same as encodeDoubleInteger, but for Float#
{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger i e = double2Float# (encodeDoubleInteger i e)



{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d =
#if WORD_SIZE_IN_BITS == 64
  case decodeDouble_2Int# d of
    (# mantSign, mantHigh, mantLow, exp #) ->
      let mant = (uncheckedShiftL# mantHigh HIGH_HALF_SHIFT#) `or#` mantLow
          bn = wordToBigNum mant
          int =
            case isTrue# (mantSign ==# 1#) of
              True -> bigNumToInteger bn
              False -> bigNumToNegInteger bn
      in
        (# int, exp #)
#elif WORD_SIZE_IN_BITS == 32
  case decodeDouble_2Int# d of
   (# mantSign, mantHigh, mantLow, exp #) ->
       (# (smallInteger mantSign) `timesInteger`
          (  (wordToInteger mantHigh `timesInteger` twoToTheThirtytwoInteger)
             `plusInteger` wordToInteger mantLow),
          exp #)
#endif

{-# NOINLINE decodeFloatInteger #-}
decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger f = case decodeFloat_Int# f of
                       (# mant, exp #) -> (# smallInteger mant, exp #)


-- TODO(SN) implement
{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

-- ** Arithmetic operations
plusInteger :: Integer -> Integer -> Integer
plusInteger (S# (INT_MINBOUND#)) (S# (INT_MINBOUND#)) = Bn# (wordToBigNum2 (int2Word# 1#) (int2Word# 0#))
plusInteger (S# x#) (S# y#) =
  case addIntC# x# y# of
    (# z#, 0# #) -> S# z#
    (# z#, _ #)
      | isTrue# (z# >=# 0#) -> Bn# (wordToBigNum (int2Word# (negateInt# z#)))
      | True -> Bp# (wordToBigNum (int2Word# z#))
plusInteger (S# x) (Bp# y)
  | isTrue# (x >=# 0#) = Bp# (plusBigNumWord y (int2Word# x))
  | True = bigNumToInteger (minusBigNumWord y (int2Word# (negateInt# x)))
plusInteger (S# x) (Bn# y)
  | isTrue# (x >=# 0#) = bigNumToNegInteger (minusBigNumWord y (int2Word# x))
  | True = Bn# (plusBigNumWord y (int2Word# (negateInt# x)))
plusInteger b (S# x) = plusInteger (S# x) b
plusInteger (Bp# x) (Bp# y) = Bp# (plusBigNum x y)
plusInteger (Bn# x) (Bn# y) = Bn# (plusBigNum x y)
plusInteger (Bp# x) (Bn# y) = case minusBigNum x y of
  (bn, False) -> bigNumToInteger bn
  (bn, True) -> bigNumToNegInteger bn
plusInteger (Bn# x) (Bp# y) = plusInteger (Bp# y) (Bn# x)

minusInteger :: Integer -> Integer -> Integer
minusInteger (S# x) (S# y) =
  case subIntC# x y of
    (# z#, 0# #) -> S# z#
    (# z#, _ #)
      | isTrue# (z# >=# 0#) -> Bn# (wordToBigNum (int2Word# (negateInt# z#)))
      | True -> Bp# (wordToBigNum (int2Word# z#))
minusInteger (Bp# x) (S# y)
  | isTrue# (y >=# 0#) = bigNumToInteger (minusBigNumWord x (int2Word# y))
  | True = Bp# (plusBigNumWord x (int2Word# (negateInt# y)))
minusInteger (Bn# x) (S# y)
  | isTrue# (y >=# 0#) = Bn# (plusBigNumWord x (int2Word# y))
  | True = bigNumToNegInteger (minusBigNumWord x (int2Word# (negateInt# y)))
minusInteger (S# x) (Bp# y) = plusInteger (S# x) (Bn# y)
minusInteger (S# x) (Bn# y) = plusInteger (S# x) (Bp# y)
minusInteger (Bp# x) (Bp# y) = plusInteger (Bp# x) (Bn# y)
minusInteger (Bp# x) (Bn# y) = Bp# (plusBigNum x y)
minusInteger (Bn# x) (Bp# y) = Bn# (plusBigNum x y)
minusInteger (Bn# x) (Bn# y) = plusInteger (Bp# y) (Bn# x)

-- | Switch sign of Integer.
negateInteger :: Integer -> Integer
negateInteger (Bn# n) = Bp# n
negateInteger (S# INT_MINBOUND#) = Bp# (wordToBigNum ABS_INT_MINBOUND##)
negateInteger (S# i#) = S# (negateInt# i#)
negateInteger (Bp# bn)
  | isTrue# (eqBigNumWord# bn ABS_INT_MINBOUND##) = S# INT_MINBOUND#
  | True = Bn# bn
{-# NOINLINE negateInteger #-}

-- Absolute value
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND#) = Bp# (wordToBigNum ABS_INT_MINBOUND##)
absInteger (S# i)
  | isTrue# (i >=# 0#) = S# i
  | True = S# (negateInt# i)
absInteger (Bp# bn) = Bp# bn
absInteger (Bn# bn) = Bp# bn

-- TODO(SN) implement
signumInteger :: Integer -> Integer
signumInteger (S# 0#) = (S# 0#)
signumInteger (S# i)
  | isTrue# (i ># 0#) = (S# 1#)
  | True = (S# (-1#))
signumInteger (Bp# bn)
  | isTrue# (isZeroBigNum# bn) = (S# 0#)
  | True = (S# 1#)
signumInteger (Bn# bn) = (S# (-1#))

-- | Integer multiplication.
timesInteger :: Integer -> Integer -> Integer
timesInteger _ (S# 0#) = S# 0#
timesInteger (S# 0#) _ = S# 0#
timesInteger x (S# 1#) = x
timesInteger (S# 1#) y = y
timesInteger x (S# -1#) = negateInteger x
timesInteger (S# -1#) y = negateInteger y
timesInteger (S# x#) (S# y#) =
  case mulIntMayOflo# x# y# of
    0# -> S# (x# *# y#)
    _  -> timesInt2Integer x# y#
timesInteger x@(S# _) y = timesInteger y x
timesInteger (Bp# x) (Bp# y) = Bp# (timesBigNum x y)
timesInteger (Bp# x) (Bn# y) = Bn# (timesBigNum x y)
timesInteger (Bp# x) (S# y#)
  | isTrue# (y# >=# 0#) = Bp# (timesBigNumWord x (int2Word# y#))
  | True = Bn# (timesBigNumWord x (int2Word# (negateInt# y#)))
timesInteger (Bn# x) (Bn# y) = Bp# (timesBigNum x y)
timesInteger (Bn# x) (Bp# y) = Bn# (timesBigNum x y)
timesInteger (Bn# x) (S# y#)
  | isTrue# (y# >=# 0#) =  Bn# (timesBigNumWord x (int2Word# y#))
  | True = Bp# (timesBigNumWord x (int2Word# (negateInt# y#)))

-- | Construct 'Integer' from the product of two 'Int#'s
timesInt2Integer :: Int# -> Int# -> Integer
timesInt2Integer x# y# =
  case (# isTrue# (x# >=# 0#), isTrue# (y# >=# 0#) #) of
    (# False, False #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)

    (# True, False #) -> case timesWord2# (int2Word# x#) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# False, True #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# y#) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# True, True #) -> case timesWord2# (int2Word# x#) (int2Word# y#) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)

-- | Integer division rounded to zero, calculating 'quotInteger' and
-- 'remInteger'. Divisor must be non-zero or a division-by-zero will be raised.
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger n (S# 1#) = (# n, S# 0# #)
quotRemInteger n (S# -1#) =
  let !q = negateInteger n
  in  (# q, S# 0# #)
quotRemInteger _ (S# 0#) = -- will raise division by zero
  (# S# (quotInt# 0# 0#), S# (remInt# 0# 0#) #)
quotRemInteger (S# 0#) _ = (# S# 0#, S# 0# #)
quotRemInteger (S# n#) (S# d#) =
  let (# q#, r# #) = quotRemInt# n# d#
  in  (# S# q#, S# r# #)
quotRemInteger (Bp# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToInteger r #)
quotRemInteger (Bp# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToInteger r #)
quotRemInteger (Bn# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToNegInteger r #)
quotRemInteger (Bn# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToNegInteger r #)
quotRemInteger (Bp# n) (S# d#)
  | isTrue# (d# >=# 0#) = let (# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToInteger q, inline wordToInteger r# #)
  | True = let (# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToNegInteger q, inline wordToInteger r# #)
quotRemInteger (Bn# n) (S# d#)
  | isTrue# (d# >=# 0#) = let (# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToNegInteger q, inline wordToNegInteger r# #)
  | True = let (# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToInteger q, inline wordToNegInteger r# #)
quotRemInteger i@(S# _) (Bn# d) = (# S# 0#, i #) -- since i < d
quotRemInteger i@(S# i#) (Bp# d) -- need to account for (S# minBound)
    | isTrue# (i# ># 0#) = (# S# 0#, i #)
    | isTrue# (gtBigNumWord# d (int2Word# (negateInt# i#))) = (# S# 0#, i #)
    | True {- abs(i) == d -} = (# S# -1#, S# 0# #)
{-# NOINLINE quotRemInteger #-}

-- TODO(SN) implement
quotInteger :: Integer -> Integer -> Integer
quotInteger _ _ = undefined

-- TODO(SN) implement
remInteger :: Integer -> Integer -> Integer
remInteger _ _ = undefined

-- TODO(SN) implement
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger _ _ = (# undefined, undefined #)

-- TODO(SN) implement
divInteger :: Integer -> Integer -> Integer
divInteger _ _ = undefined

-- TODO(SN) implement
modInteger :: Integer -> Integer -> Integer
modInteger _ _ = undefined

-- ** Bit operations

-- TODO(SN) implement
complementInteger :: Integer -> Integer
complementInteger _ = undefined

-- TODO(SN) implement
andInteger :: Integer -> Integer -> Integer
andInteger _ = undefined

-- TODO(SN) implement
xorInteger :: Integer -> Integer -> Integer
xorInteger _ _ = undefined

-- | Bitwise OR of Integers.
orInteger :: Integer -> Integer -> Integer
-- short-cuts
orInteger (S# 0#) y = y
orInteger x (S# 0#) = x
orInteger x@(S# -1#) _ = x
orInteger _ y@(S# -1#) = y
-- base-cases
orInteger (S# a#) (S# b#) = S# (a# `orI#` b#)
orInteger (Bp# x) (Bp# y) = Bp# (orBigNum x y)
orInteger (Bn# x) (Bn# y) =
  bigNumToNegInteger (minusBigNumWord (andBigNum (plusBigNumWord x 1##)
                                                   (plusBigNumWord y 1##)) 1##)
orInteger x@(Bn# _) y@(Bp# _) = orInteger y x -- swap for next case
orInteger (Bp# x) (Bn# y) =
  bigNumToNegInteger (minusBigNumWord (andnBigNum (plusBigNumWord y 1##) x) 1##)
-- -- TODO/FIXpromotion-hack
orInteger  x@(S# _) y = orInteger (unsafePromote x) y
orInteger  x  y@(S# _) = orInteger x (unsafePromote y)
{-# NOINLINE orInteger #-}

-- HACK warning! breaks invariant on purpose
unsafePromote :: Integer -> Integer
unsafePromote (S# x#)
    | isTrue# (x# >=# 0#) = Bp# (wordToBigNum (int2Word# x#))
    | True                = Bn# (wordToBigNum (int2Word# (negateInt# x#)))
unsafePromote x = x

-- | Shift-left operation. Undefined for negative shift amount ('Int#').
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger x 0# = x
shiftLInteger (S# 0#) _  = S# 0#
-- TODO(SN) re-enable shiftLInteger (S# 1#) n# = bitInteger n#
shiftLInteger (S# i#) n#
  | isTrue# (i# >=# 0#) = bigNumToInteger (shiftLBigNum (wordToBigNum (int2Word# i#)) n#)
  | True = bigNumToNegInteger (shiftLBigNum (wordToBigNum (int2Word# (negateInt# i#))) n#)
shiftLInteger (Bp# bn) n# = Bp# (shiftLBigNum bn n#)
shiftLInteger (Bn# bn) n# = Bn# (shiftLBigNum bn n#)
{-# NOINLINE shiftLInteger #-}

-- TODO(SN) implement
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger _ _ = undefined

-- TODO(SN) implement
testBitInteger :: Integer -> Int# -> Bool
testBitInteger _ _ = undefined

-- ** Comparison

-- TODO(SN) implement
compareInteger :: Integer -> Integer -> Ordering
compareInteger _ _ = undefined

-- | Equal operation for Integers. Returns 0# as false and 1# as True
eqInteger# :: Integer -> Integer -> Int#
eqInteger# (S# i1) (S# i2) = i1 ==# i2
eqInteger# (Bp# bn1) (Bp# bn2) = bnEq bn1 bn2
eqInteger# (Bn# bn1) (Bn# bn2) = bnEq bn1 bn2
eqInteger# _ _ = 0#

bnEq :: BigNum -> BigNum -> Int#
bnEq bn1 bn2 =
  let wib1 = wordsInBigNum# bn1
      wib2 = wordsInBigNum# bn2 in
  case isTrue# (wib1 ==# wib2) of
    True  -> bnEq2 (wib1 -# 1#) (wib2 -# 1#)
    False -> 0#
  where
    bnEq2 0# 0# = bigNumIdx bn1 0# `eqWord#` bigNumIdx bn2 0#
    bnEq2 0# _ = 0#
    bnEq2 _ 0# = 0#
    bnEq2 !idx1 !idx2 =
      let w1 = bigNumIdx bn1 idx1
          w2 = bigNumIdx bn2 idx2 in
      case isTrue# (w1 `eqWord#` w2) of
        True  -> bnEq2 (idx1 -# 1#) (idx2 -# 1#)
        False ->  0#


-- | Not-equal operation for Integers. Returns 0# as false and 1# as True
neqInteger# :: Integer -> Integer -> Int#
neqInteger# i1 i2 =
  case eqInteger# i1 i2 of
    0# -> 1#
    1# -> 0#
    _  -> 0#

-- TODO(SN) implement
geInteger# :: Integer -> Integer -> Int#
geInteger# _ _ = case undefined of _ -> 0#

-- TODO(SN) implement
gtInteger# :: Integer -> Integer -> Int#
gtInteger# _ _ = case undefined of _ -> 0#

-- TODO(SN) implement
leInteger# :: Integer -> Integer -> Int#
leInteger# _ _ = case undefined of _ -> 0#

-- TODO(SN) implement
ltInteger# :: Integer -> Integer -> Int#
ltInteger# _ _ = case undefined of _ -> 0#

-- * BigNum functions

-- | OpenSSL BIGNUM represented by an absolute magnitude as 'Word#' in a
-- 'ByteArray#'. It corresponds to the 'd' array in libcrypto's bignum_st
-- structure. Length is always a multiple of Word#, least-significant first
-- (BN_BITS2 == 'WORD_SIZE_IN_BITS').
data BigNum = BN# ByteArray#

-- | Mutable variant of BigNum for internal use.
data MutableBigNum s = MBN# (MutableByteArray# s)

-- | Create a MutableBigNum with given count of words.
newBigNum :: Int# -> S s (MutableBigNum s)
newBigNum count# s =
  -- Calculate byte size using shifts, e.g. for 64bit systems:
  -- total bytes = word count * 8 = word count * 2 ^ 3 = word count << 3
  case newByteArray# (count# `uncheckedIShiftL#` WORD_SHIFT#) s of
    (# s', mba# #) -> (# s', MBN# mba# #)

-- | Freeze a MutableBigNum into a BigNum.
freezeBigNum :: MutableBigNum s -> S s BigNum
freezeBigNum (MBN# mba#) s =
  case unsafeFreezeByteArray# mba# s of
    (# s', ba# #) -> (# s', BN# ba# #)

-- | Shrink a MutableBigNum the the given count of Word#.
shrinkBigNum :: MutableBigNum s -> Int# -> S s (MutableBigNum s)
shrinkBigNum mba@(MBN# mba#) 0# s =
  -- BigNum always holds min one word, but clear it in this case
  case shrinkBigNum mba 1# s of { (# s1, mba' #) ->
  case writeWordArray# mba# 0# 0## s1 of { s2 ->
  (# s2, mba #)
  }}
shrinkBigNum mba@(MBN# mba#) n# s
  | isTrue# (actual# ==# desired#) = (# s', mba #)
  | True = case shrinkMutableByteArray# mba# desired# s' of s'' -> (# s'', mba #)
 where
  !(# s', actual# #) = getSizeofMutableByteArray# mba# s
  desired# = n# `uncheckedIShiftL#` WORD_SHIFT#

-- | Write a word to a MutableBigNum at given word index. Size is not checked!
writeBigNum :: MutableBigNum s -> Int# -> Word# -> S s ()
writeBigNum (MBN# mba#) i# w# s =
  let s' = writeWordArray# mba# i# w# s
  in  (# s', () #)

-- | Copy magnitude from given BigNum into MutableBigNum.
copyBigNum :: BigNum -> MutableBigNum s -> S s ()
copyBigNum a@(BN# ba#) (MBN# mbb#) =
  copyWordArray# ba# 0# mbb# 0# (wordsInBigNum# a)

zeroBigNum :: BigNum
zeroBigNum = runS (newBigNum 1# >>= freezeBigNum)

-- | Create a BigNum from a single Word#.
wordToBigNum :: Word# -> BigNum
wordToBigNum w# = runS $ do
  mbn <- newBigNum 1#
  writeBigNum mbn 0# w#
  freezeBigNum mbn

-- | Create a BigNum from two Word#.
wordToBigNum2 :: Word# -- ^ High word
              -> Word# -- ^ low word
              -> BigNum
wordToBigNum2 h# l# = runS $ do
  mbn <- newBigNum 2#
  writeBigNum mbn 0# l#
  writeBigNum mbn 1# h#
  freezeBigNum mbn

-- | Truncate a BigNum to a single Word#.
bigNumToWord :: BigNum -> Word#
bigNumToWord (BN# ba) = indexWordArray# ba 0#

bigNumIdx :: BigNum -> Int# -> Word#
bigNumIdx (BN# ba) idx = indexWordArray# ba idx

-- | Truncate a BigNum to a single Int#
bigNumToInt :: BigNum -> Int#
bigNumToInt (BN# ba) = indexIntArray# ba 0#

-- | Create a positive Integer from given BigNum. Converts to small Integer if possible.
bigNumToInteger :: BigNum -> Integer
bigNumToInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# >=# 0#)) = S# i#
  | True = Bp# bn
  where
    i# = word2Int# (bigNumToWord bn)

-- | Create a negative Integer from given BigNum. Converts to small Integer if possible.
bigNumToNegInteger :: BigNum -> Integer
bigNumToNegInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# <=# 0#)) = S# i#
  | True = Bn# bn
  where
    i# = negateInt# (word2Int# (bigNumToWord bn))

-- ** Comparisons

-- | Get number of 'Word#' in 'BigNum'. See 'newBigNum' for shift explanation.
wordsInBigNum# :: BigNum -> Int#
wordsInBigNum# (BN# ba#) = (sizeofByteArray# ba#) `uncheckedIShiftRL#` WORD_SHIFT#

-- | Return @1#@ iff BigNum holds one 'Word#' equal to given 'Word#'.
eqBigNumWord# :: BigNum -> Word# -> Int#
eqBigNumWord# bn w# =
  (wordsInBigNum# bn ==# 1#) `andI#` (bigNumToWord bn `eqWord#` w#)

-- | Return @1#@ iff BigNum holds one 'Word#' equal to 0##
isZeroBigNum# :: BigNum -> Int#
isZeroBigNum# bn =
  (wordsInBigNum# bn ==# 1#) `andI#` (bigNumToWord bn `eqWord#` 0##)


-- | Return @1#@ iff BigNum is greater than a given 'Word#'.
gtBigNumWord# :: BigNum -> Word# -> Int#
gtBigNumWord# bn w# =
  (wordsInBigNum# bn ># 1#) `orI#` (bigNumToWord bn `gtWord#` w#)

-- ** Bit-operations

-- | Bitwise OR of two BigNum.
orBigNum :: BigNum -> BigNum -> BigNum
orBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = y
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = orBigNum' x# y# nx# ny#
  | True = orBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  orBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# or# m#
    case isTrue# (n# ==# m#) of
      False -> copyWordArray# a# m# mba# m# (n# -# m#)
      True  -> return ()
    freezeBigNum mbn

-- | Bitwise AND of two BigNum.
andBigNum :: BigNum -> BigNum -> BigNum
andBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = zeroBigNum
  | isTrue# (nx# >=# ny#) = andBigNum' x# y# nx# ny#
  | True = andBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  andBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# and# m#
    freezeBigNum mbn -- TODO(SN): resize mbn if possible

-- | Bitwise ANDN (= AND . NOT) of two BigNum, resulting BigNum is positive.
andnBigNum :: BigNum -> BigNum -> BigNum
andnBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = andnBigNum' x# y# nx# ny#
  | True = andnBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m# -- TODO(SN): test as gmp does something different?
  andnBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# (\a b -> a `and#` (not# b)) m#
    freezeBigNum mbn -- TODO(SN): resize mbn if possible

-- | Shift left logical, undefined for negative Int#.
shiftLBigNum :: BigNum -> Int# -> BigNum
shiftLBigNum x 0# = x
shiftLBigNum x _
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
shiftLBigNum a@(BN# ba#) c# = runS $ do
  r@(MBN# mbq#) <- newBigNum nq#
  (I# i#) <- liftIO (bn_lshift mbq# nq# ba# na# c#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nq# = na# +# nwords# +# 1#
  nwords# = quotInt# c# WORD_SIZE_IN_BITS#

-- size_t integer_bn_lshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n) {
foreign import ccall unsafe "integer_bn_lshift"
  bn_lshift :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO Int

-- ** Arithmetic operations

plusBigNum :: BigNum -> BigNum -> BigNum
plusBigNum a@(BN# a#) b@(BN# b#) = runS $ do
    r@(MBN# mbr#) <- newBigNum nr#
    (I# i#) <- liftIO (bn_add mbr# nr# a# na# b# nb#)
    shrinkBigNum r i# >>= freezeBigNum
  where
    na# = wordsInBigNum# a
    nb# = wordsInBigNum# b
    nr# = (maxInt# na# nb#) +# 1#

minusBigNum :: BigNum -> BigNum -> (BigNum, Bool)
minusBigNum a@(BN# a#) b@(BN# b#) = runS $ do
    r@(MBN# mbr#) <- newBigNum nr#
    ba@(BA# negValue#) <- newByteArray 4#
    (I# i#) <- liftIO (bn_sub mbr# nr# a# na# b# nb# negValue#)
    (I# neg#) <- readInt32ByteArray ba
    bn <- shrinkBigNum r i# >>= freezeBigNum
    return (bn, isTrue# neg#)
  where
    na# = wordsInBigNum# a
    nb# = wordsInBigNum# b
    nr# = maxInt# na# nb#

foreign import ccall unsafe "integer_bn_add"
  bn_add :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> IO Int

foreign import ccall unsafe "integer_bn_sub"
  bn_sub :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> IO Int

maxInt# :: Int# -> Int# -> Int#
maxInt# x# y#
  | isTrue# (x# >=# y#) = x#
  | True = y#

-- | Add given Word# to BigNum.
plusBigNumWord :: BigNum -> Word# -> BigNum
plusBigNumWord a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  copyBigNum a r
  (I# i#) <- liftIO (bn_add_word mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a
   nr# = na# +# 1#

-- size_t integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_add_word"
  bn_add_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Subtract given Word# from BigNum.
minusBigNumWord :: BigNum -> Word# -> BigNum
minusBigNumWord a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum na#
  copyBigNum a r
  (I# i#) <- liftIO (bn_sub_word mbr# na# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a

-- size_t integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_sub_word"
  bn_sub_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply given BigNum with given Word#.
timesBigNumWord :: BigNum -> Word# -> BigNum
timesBigNumWord a@(BN# ba#) w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  copyBigNum a r
  (I# i#) <- liftIO (bn_mul_word mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nr# = na# +# 1#

-- int integer_bn_mul_word(BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_mul_word"
  bn_mul_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply two BigNums.
timesBigNum :: BigNum -> BigNum -> BigNum
timesBigNum a@(BN# a#) b@(BN# b#) = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  (I# i#) <- liftIO (bn_mul mbr# nr# a# na# b# nb#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nb# = wordsInBigNum# b

  -- OpenSSL's BN_mul requires result BigNum to be big enough (bn_mul.c:553ff)
  nr# = case isTrue# (na# >=# nb#) of
    True  -> calculateSize na#
    False -> calculateSize nb#

  calculateSize n# =
    let j = lowerPowerTwo# n#
        k = j +# j
    in case isTrue# (k ># n#) of
      True -> k *# 4#
      False -> k *# 2#

  lowerPowerTwo# i# = 1# `uncheckedIShiftL#` (numBitsWord# (int2Word# i#) -# 1#)

  numBitsWord# w# = WORD_SIZE_IN_BITS# -# (word2Int# (clz# w#))

-- int integer_bn_mul(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, BN_ULONG *bb, size_t bsize)
foreign import ccall unsafe "integer_bn_mul"
  bn_mul :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> IO Int

-- | Divide a BigNum by a Word#, returning the quotient and a remainder (rounded
-- to zero). The divisor must not be 0##.
quotRemBigNumWord :: BigNum -> Word# -> (# BigNum, Word# #)
quotRemBigNumWord a 0## = (# a, remWord# 0## 0## #) -- raises division by zero
quotRemBigNumWord a 1## = (# a, 0## #)
quotRemBigNumWord a@(BN# ba#) w# = case runS divWord of (q, (I# r#)) -> (# q, int2Word# r# #)
 where
  na# = wordsInBigNum# a
  nq# = na# +# 1#
  divWord = do
    q@(MBN# mbq#) <- newBigNum nq#
    copyBigNum a q
    qtopba@(BA# qtopba#) <- newByteArray 4#
    -- TODO(SN): div_word correct?
    r@(I# r#) <- liftIO (bn_div_word mbq# nq# w# qtopba#)
    (I# qtop#) <- readInt32ByteArray qtopba
    q' <- shrinkBigNum q qtop# >>= freezeBigNum
    return (q', r)

-- int integer_bn_div_word(BN_ULONG *qb, size_t qsize, BN_ULONG w, int32_t *qtop)
foreign import ccall unsafe "integer_bn_div_word"
  bn_div_word :: MutableByteArray# s -> Int# -> Word# -> ByteArray# -> IO Int

-- | Divide a BigNum by another BigNum, returning the quotient and a remainder
-- (rounded to zero). The divisor must not be 0##.
quotRemBigNum :: BigNum -> BigNum -> (# BigNum, BigNum #)
quotRemBigNum a@(BN# ba#) d@(BN# bd#) = case runS div of (q, r) -> (# q, r #)
 where
  na# = wordsInBigNum# a
  nd# = wordsInBigNum# d
  nq# = na# +# 1#
  nr# = na#
  div = do
    q@(MBN# mbq#) <- newBigNum nq#
    r@(MBN# mbqtop#) <- newBigNum nr#
    qtopba@(BA# qtopba#) <- newByteArray 4#
    rtopba@(BA# rtopba#) <- newByteArray 4#
    liftIO (bn_div mbq# nq# mbqtop# nr# ba# na# bd# nd# qtopba# rtopba#)
    (I# qtop#) <- readInt32ByteArray qtopba
    (I# rtop#) <- readInt32ByteArray rtopba
    q' <- shrinkBigNum q qtop# >>= freezeBigNum
    r' <- shrinkBigNum r rtop# >>= freezeBigNum
    return (q', r')

-- int integer_bn_div(BN_ULONG *qb, size_t qsize,
--                    BN_ULONG *remb, size_t remsize,
--                    BN_ULONG *ab, size_t asize,
--                    BN_ULONG *db, size_t dsize,
--                    int32_t* qtop, int32_t* remtop)
foreign import ccall unsafe "integer_bn_div"
  bn_div :: MutableByteArray# s -> Int#
         -> MutableByteArray# s -> Int#
         -> ByteArray# -> Int#
         -> ByteArray# -> Int#
         -> ByteArray# -> ByteArray#
         -> IO Int

-- * ByteArray# utilities

data ByteArray = BA# ByteArray#

-- | Helper to allocate a freezed block of memory and return a lifted type for
-- easier handling.
newByteArray :: Int# -> S s ByteArray
newByteArray i# s =
  let (# s1, mba# #) = newPinnedByteArray# i# s
      (# s2, ba# #) = unsafeFreezeByteArray# mba# s1
  in  (# s2, BA# ba# #)

readInt32ByteArray :: ByteArray -> S s Int
readInt32ByteArray (BA# ba#) s = (# s, I# (indexInt32Array# ba# 0#) #)

-- | Copy multiples of Word# between ByteArray#s with offsets in words.
copyWordArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> S s ()
copyWordArray# src srcOffset dst dstOffset len s =
  let s' = copyByteArray# src srcOffsetBytes dst dstOffsetBytes lenBytes s
  in  (# s', () #)
 where
  srcOffsetBytes = srcOffset `uncheckedIShiftL#` WORD_SHIFT#
  dstOffsetBytes = dstOffset `uncheckedIShiftL#` WORD_SHIFT#
  lenBytes = len `uncheckedIShiftL#` WORD_SHIFT#

-- | Map over two ByteArray# for given number of words and store result in
-- MutableByteArray#.
mapWordArray# :: ByteArray# -> ByteArray# -> MutableByteArray# s
              -> (Word# -> Word# -> Word#)
              -> Int# -- ^ Number of words
              -> S s ()
mapWordArray# _ _ _ _ -1# s = (# s, () #)
mapWordArray# a# b# mba# f i# s =
  let w# = f (indexWordArray# a# i#) (indexWordArray# b# i#)
  in  case writeWordArray# mba# i# w# s of
        s' -> mapWordArray# a# b# mba# f (i# -# 1#) s'

-- * Borrowed things

-- ** From integer-gmp (requires -XRebindableSyntax):
-- monadic combinators for low-level state threading

type S s a = State# s -> (# State# s, a #)

infixl 1 >>=
infixl 1 >>
infixr 0 $

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

{-# INLINE (>>=) #-}
(>>=) :: S s a -> (a -> S s b) -> S s b
(>>=) m k = \s -> case m s of (# s', a #) -> k a s'

{-# INLINE (>>) #-}
(>>) :: S s a -> S s b -> S s b
(>>) m k = \s -> case m s of (# s', _ #) -> k s'

{-# INLINE return #-}
return :: a -> S s a
return a = \s -> (# s, a #)

{-# INLINE liftIO #-}
liftIO :: IO a -> S RealWorld a
liftIO (IO m) = m

-- NB: equivalent of GHC.IO.unsafeDupablePerformIO, see notes there
runS :: S RealWorld a -> a
runS m = case runRW# m of (# _, a #) -> a

-- stupid hack
fail :: [Char] -> S s a
fail s = return (raise# s)

-- ** From Base

{-# INLINE (.) #-}
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- ** From GHC.Err:

undefined :: forall a. a
undefined = runS $ fail "Prelude.undefined"
