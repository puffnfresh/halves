{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Data.Halves (
  Halves(..)
, Halves'
, quarters
, eighths

, upperHalf
, lowerHalf
, swappedHalves

, collectHalves
, collectQuarters
, collectEighths

, finiteBitHalves
) where

import           Control.Lens
import           Data.Bits
import           Data.Halves.FiniteBits
import           Data.Halves.Tuple
import           Data.Int
import           Data.Word

class Halves s t a b | s -> a, t -> b, s -> t, t -> s where
  halves :: Iso s t (Pair2 a) (Pair2 b)

type Halves' s a
  = Halves s s a a

-- >>> (300 :: Word16) ^. halves
-- (1,44)
-- >>> (1 :: Word8, 44 :: Word8) ^. from halves
-- 300
instance Halves Word16 Word16 Word8 Word8 where
  halves =
    finiteBitHalves

-- >>> (65538 :: Word32) ^. halves
-- (1,2)
-- >>> (1 :: Word16, 2 :: Word16) ^. from halves
-- 65538
instance Halves Word32 Word32 Word16 Word16 where
  halves =
    finiteBitHalves

-- >>> (4294967299 :: Word64) ^. halves
-- (1,3)
-- >>> (1 :: Word32, 3 :: Word32) ^. from halves
-- 4294967299
instance Halves Word64 Word64 Word32 Word32 where
  halves =
    finiteBitHalves

-- >>> (-30748 :: Int16) ^. halves
-- (-121,-28)
-- >>> ((-121) :: Int8, (-28) :: Int8) ^. from halves
-- -30748
instance Halves Int16 Int16 Int8 Int8 where
  halves =
    finiteBitHalves

-- >>> (-1610312736 :: Int32) ^. halves
-- (-24572,-27680)
-- >>> (-24572 :: Int16, -27680 :: Int16) ^. from halves
-- -1610312736
instance Halves Int32 Int32 Int16 Int16 where
  halves =
    finiteBitHalves

-- >>> (-6917529027641081356 :: Int64) ^. halves
-- (-1610612736,500)
-- >>> (-1610612736 :: Int32, 500 :: Int32) ^. from halves
-- -6917529027641081356
instance Halves Int64 Int64 Int32 Int32 where
  halves =
    finiteBitHalves

-- >>> (3201205369 :: Word32) ^. quarters
-- (190,206,132,121)
-- >>> (190 :: Word8, 206 :: Word8, 132 :: Word8, 121 :: Word8) ^. from quarters
-- 3201205369
quarters ::
  (Halves a b' b b, Halves s t a b') =>
  Iso s t (Pair4 b) (Pair4 b)
quarters =
  halves . bimapping halves halves . tuple4

-- >>> (13832053055282163709 :: Word64) ^. eighths
-- (191,245,82,247,234,115,47,253)
eighths ::
  (Halves a' b' b b, Halves a b'' a' b', Halves s t a b'') =>
  Iso s t (Pair8 b) (Pair8 b)
eighths =
  halves . bimapping quarters quarters . tuple8

-- >>> (4294967299 :: Word64) ^. upperHalf
-- 1
upperHalf ::
  (Halves s t a a) =>
  Lens s t a a
upperHalf =
  halves . _1

-- >>> (4294967299 :: Word64) ^. lowerHalf
-- 3
lowerHalf ::
  (Halves s t a a) =>
  Lens s t a a
lowerHalf =
  halves . _2

-- >>> (4294967299 :: Word64) ^. swappedHalves
-- 12884901889
swappedHalves ::
  (Halves s t a a) =>
  Iso s t t s
swappedHalves =
  halves . swapped . from halves

-- >>> collectHalves ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [258,772,1286,1800]
collectHalves ::
  (Halves' b a) =>
  [a] ->
  [b]
collectHalves (a:b:xs) =
  (a, b) ^. from halves : collectHalves xs
collectHalves _ =
  []

-- >>> collectQuarters ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [16909060,84281096]
collectQuarters ::
  (Halves s t a a, Halves b b s t) =>
  [a] ->
  [b]
collectQuarters (a:b:c:d:xs) =
  (a, b, c, d) ^. from quarters : collectQuarters xs
collectQuarters _ =
  []

-- >>> collectEighths ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [72623859790382856]
collectEighths ::
  (Halves a' b' a a, Halves t' b'' a' b', Halves b b t' b'') =>
  [a] ->
  [b]
collectEighths (a:b:c:d:e:f:g:h:xs) =
  (a, b, c, d, e, f, g, h) ^. from eighths : collectEighths xs
collectEighths _ =
  []

finiteBitHalves ::
  forall a b c.
  (Integral a, Integral b, Integral c, Bits a, AsFiniteBits' b c) =>
  Iso' a (Pair2 b)
finiteBitHalves =
  iso f g
  where
    s =
      finiteBitSize (zeroBits :: c)
    f a =
      (fromIntegral (unsafeShiftR a s), fromIntegral a)
    g (a, b) =
      unsafeShiftL (fromIntegral a) s .|. fromIntegral (b ^. asFiniteBits)
