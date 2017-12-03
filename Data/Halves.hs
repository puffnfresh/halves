{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Data.Halves (
  Halves(..)
, quarters
, eighths

, upperHalf
, lowerHalf
, swappedHalves

, collectHalves
, collectQuarters
, collectEighths

, finiteBitsHalves
) where

import           Control.Lens
import           Data.Bits
import           Data.Word

class Halves a b | a -> b, b -> a where
  halves :: Iso' a (b, b)

-- >>> (300 :: Word16) ^. halves
-- (1,44)
-- >>> (1 :: Word8, 44 :: Word8) ^. from halves
-- 300
instance Halves Word16 Word8 where
  halves =
    finiteBitsHalves

-- >>> (65538 :: Word32) ^. halves
-- (1,2)
-- >>> (1 :: Word16, 2 :: Word16) ^. from halves
-- 65538
instance Halves Word32 Word16 where
  halves =
    finiteBitsHalves

-- >>> (4294967299 :: Word64) ^. halves
-- (1,3)
-- >>> (1 :: Word32, 3 :: Word32) ^. from halves
-- 4294967299
instance Halves Word64 Word32 where
  halves =
    finiteBitsHalves

-- >>> (3201205369 :: Word32) ^. quarters
-- (190,206,132,121)
-- >>> (190 :: Word8, 206 :: Word8, 132 :: Word8, 121 :: Word8) ^. from quarters
-- 3201205369
quarters ::
  (Halves a b, Halves b c) =>
  Iso' a (c, c, c, c)
quarters =
  halves . bimapping halves halves . tuple4

-- >>> (13832053055282163709 :: Word64) ^. eighths
-- (191,245,82,247,234,115,47,253)
eighths ::
  (Halves a b, Halves b c, Halves c d) =>
  Iso' a (d, d, d, d, d, d, d, d)
eighths =
  halves . bimapping quarters quarters . tuple8

-- >>> (4294967299 :: Word64) ^. upperHalf
-- 1
upperHalf ::
  (Halves a b) =>
  Lens' a b
upperHalf =
  halves . _1

-- >>> (4294967299 :: Word64) ^. lowerHalf
-- 3
lowerHalf ::
  (Halves a b) =>
  Lens' a b
lowerHalf =
  halves . _2

-- >>> (4294967299 :: Word64) ^. swappedHalves
-- 12884901889
swappedHalves ::
  (Halves a b) =>
  Iso' a a
swappedHalves =
  halves . swapped . from halves

-- >>> collectHalves ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [258,772,1286,1800]
collectHalves ::
  (Halves a b) =>
  [b] ->
  [a]
collectHalves (a:b:xs) =
  (a, b) ^. from halves : collectHalves xs
collectHalves _ =
  []

-- >>> collectQuarters ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [16909060,84281096]
collectQuarters ::
  (Halves a b, Halves b c) =>
  [c] ->
  [a]
collectQuarters (a:b:c:d:xs) =
  (a, b, c, d) ^. from quarters : collectQuarters xs
collectQuarters _ =
  []

-- >>> collectEighths ([1, 2, 3, 4, 5, 6, 7, 8] :: [Word8])
-- [72623859790382856]
collectEighths ::
  (Halves a b, Halves b c, Halves c d) =>
  [d] ->
  [a]
collectEighths (a:b:c:d:e:f:g:h:xs) =
  (a, b, c, d, e, f, g, h) ^. from eighths : collectEighths xs
collectEighths _ =
  []

-- >>> (((), True), ("three", 'f')) ^. tuple4
-- ((),True,"three",'f')
-- >>> ((), True, "three", 'f') ^. from tuple4
-- (((),True),("three",'f'))
tuple4 ::
  Iso' ((a, b), (c, d)) (a, b, c, d)
tuple4 =
  iso f g
  where
    f ((a, b), (c, d)) =
      (a, b, c, d)
    g (a, b, c, d) =
      ((a, b), (c, d))

-- >>> (((), True, "three", 'f'), ('a', False, "x", ())) ^. tuple8
-- ((),True,"three",'f','a',False,"x",())
tuple8 ::
  Iso' ((a, b, c, d), (e, f, g, h)) (a, b, c, d, e, f, g, h)
tuple8 =
  iso f g
  where
    f ((a, b, c, d), (e, f', g', h)) =
      (a, b, c, d, e, f', g', h)
    g (a, b, c, d, e, f', g', h) =
      ((a, b, c, d), (e, f', g', h))

finiteBitsHalves :: forall a b. (Bits a, FiniteBits b, Integral a, Integral b) => Iso' a (b, b)
finiteBitsHalves =
  iso f g
  where
    s =
      finiteBitSize (zeroBits :: b)
    f a =
      (fromIntegral (shiftR a s), fromIntegral a)
    g (a, b) =
      shiftL (fromIntegral a) s .|. fromIntegral b
