{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Halves.FiniteBits (
  AsFiniteBits(..)
, asFiniteBitsDefault
) where

import           Control.Lens
import           Data.Bits
import           Data.Int
import           Data.Word

class FiniteBits b => AsFiniteBits a b | a -> b where
  asFiniteBits :: Iso' a b

asFiniteBitsDefault ::
  (Integral a, Integral b) =>
  Iso' a b
asFiniteBitsDefault =
    iso fromIntegral fromIntegral

instance AsFiniteBits Word8 Word8 where
  asFiniteBits =
    simple

instance AsFiniteBits Word16 Word16 where
  asFiniteBits =
    simple

instance AsFiniteBits Word32 Word32 where
  asFiniteBits =
    simple

instance AsFiniteBits Word64 Word64 where
  asFiniteBits =
    simple

instance AsFiniteBits Int8 Word8 where
  asFiniteBits =
    asFiniteBitsDefault

instance AsFiniteBits Int16 Word16 where
  asFiniteBits =
    asFiniteBitsDefault

instance AsFiniteBits Int32 Word32 where
  asFiniteBits =
    asFiniteBitsDefault

instance AsFiniteBits Int64 Word64 where
  asFiniteBits =
    asFiniteBitsDefault
