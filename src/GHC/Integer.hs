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
module GHC.Integer where

#include "MachDeps.h"

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
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

data Integer = S# !Int#
               -- ^ small integer
             | Bp# {-# UNPACK #-} !BigNum
               -- ^ positive bignum, > maxbound(Int)
             | Bn# {-# UNPACK #-} !BigNum
               -- ^ negative bignum, < minbound(Int)

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
-- Word#
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

{- 
integerToInt (S# i#)  = i#
integerToInt (Jp# bn) = bigNatToInt bn
integerToInt (Jn# bn) = negateInt# (bigNatToInt bn)
-}
-- TODO floatFromInteger :: Integer -> Float#
-- TODO doubleFromInteger :: Integer -> Double#
-- TODO encodeFloatInteger :: Integer -> Int# -> Float#
-- TODO encodeDoubleInteger :: Integer -> Int# -> Double#
-- TODO decodeDoubleInteger :: Double# -> (# Integer, Int# #)

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
  | isTrue# (x >=# 0#) = Bp# (plusBigNumWord 0# y (int2Word# x))
  | True = bigNumToInteger (minusBigNumWord 0# y (int2Word# (negateInt# x)))
plusInteger (S# x) (Bn# y)
  | isTrue# (x >=# 0#) = bigNumToNegInteger (minusBigNumWord 0# y (int2Word# x))
  | True = Bn# (plusBigNumWord 0# y (int2Word# (negateInt# x)))
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
  | isTrue# (y >=# 0#) = bigNumToInteger (minusBigNumWord 0# x (int2Word# y))
  | True = Bp# (plusBigNumWord 0# x (int2Word# (negateInt# y)))
minusInteger (Bn# x) (S# y)
  | isTrue# (y >=# 0#) = Bn# (plusBigNumWord 0# x (int2Word# y))
  | True = bigNumToNegInteger (minusBigNumWord 0# x (int2Word# (negateInt# y)))
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

-- TODO signumInteger :: Integer -> Integer

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

-- TODO quotInteger :: Integer -> Integer -> Integer
-- TODO remInteger :: Integer -> Integer -> Integer
-- TODO divModInteger :: Integer -> Integer -> (#Integer, Integer#)
-- TODO divInteger :: Integer -> Integer -> Integer
-- TODO modInteger :: Integer -> Integer -> Integer

-- ** Bit operations

-- TODO complementInteger :: Integer -> Integer
-- TODO andInteger :: Integer -> Integer -> Integer
-- TODO xorInteger :: Integer -> Integer -> Integer

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
  bigNumToNegInteger (plusBigNumWord 1# (andBigNum (minusBigNumWord 1# x 1##)
                                                   (minusBigNumWord 1# y 1##)) 1##)
orInteger x@(Bn# _) y@(Bp# _) = orInteger y x -- swap for next case
orInteger (Bp# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord 1# (andnBigNum (minusBigNumWord 1# y 1##) x) 1##)
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
-- TODO re-enable shiftLInteger (S# 1#) n# = bitInteger n#
shiftLInteger (S# i#) n#
  | isTrue# (i# >=# 0#) = bigNumToInteger (shiftLBigNum (wordToBigNum (int2Word# i#)) n#)
  | True = bigNumToNegInteger (shiftLBigNum (wordToBigNum (int2Word# (negateInt# i#))) n#)
shiftLInteger (Bp# bn) n# = Bp# (shiftLBigNum bn n#)
shiftLInteger (Bn# bn) n# = Bn# (shiftLBigNum bn n#)
{-# NOINLINE shiftLInteger #-}

-- TODO shiftRInteger :: Integer -> Int# -> Integer

-- ** Comparison

-- TODO compareInteger :: Integer -> Integer -> Ordering
-- TODO eqInteger# :: Integer -> Integer -> Int#
-- TODO neqInteger :: Integer -> Integer -> Int#
-- TODO geInteger# :: Integer -> Integer -> Int#
-- TODO gtInteger# :: Integer -> Integer -> Int#
-- TODO leInteger# :: Integer -> Integer -> Int#
-- TODO ltInteger# :: Integer -> Integer -> Int#

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
plusBigNumWord :: Int# -- ^ Sign of number, 1# if negative
               -> BigNum -> Word# -> BigNum
plusBigNumWord neg# a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  copyBigNum a r
  (I# i#) <- liftIO (bn_add_word neg# mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a
   nr# = na# +# 1#

-- size_t integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_add_word"
  bn_add_word :: Int# -> MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Subtract given Word# from BigNum.
minusBigNumWord :: Int# -- ^ Sign of number, 1# if negative
                -> BigNum -> Word# -> BigNum
minusBigNumWord neg# a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum na# -- TODO(SN): make sure enough allocated
  copyBigNum a r
  (I# i#) <- liftIO (bn_sub_word neg# mbr# na# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a

-- size_t integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_sub_word"
  bn_sub_word :: Int# -> MutableByteArray# s -> Int# -> Word# -> IO Int

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
quotRemBigNumWord a@(BN# ba#) w# = divWord
 where
  na# = wordsInBigNum# a
  nq# = na# +# 1#
  divWord =
    case newBigNum nq# realWorld# of { (# s1, q@(MBN# mbq#) #) ->
    case copyBigNum a q s1 of { (# s2, () #) ->
    case newByteArray# 4# s2 of { (# s3, mbqtop# #) ->
    case liftIO (bn_div_word mbq# nq# w# mbqtop#) s3 of { (# s4, (I# r#) #) ->
    case readInt32Array# mbqtop# 0# s4 of { (# s5, qtop# #) ->
    case shrinkBigNum q qtop# s5 of { (# s6, q' #) ->
    case freezeBigNum q' s4 of { (# s5, q'' #) ->
    (# q'', int2Word# r# #)
    }}}}}}}

-- int integer_bn_div_word(BN_ULONG *qb, size_t qsize, BN_ULONG w, int32_t *qtop)
foreign import ccall unsafe "integer_bn_div_word"
  bn_div_word :: MutableByteArray# s -> Int# -> Word# -> MutableByteArray# s -> IO Int

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
