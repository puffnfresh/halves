{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Lens
import           Control.Monad  (unless)
import           Data.Halves
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           System.Exit    (exitFailure)
import           System.IO

isoToFrom ::
  (MonadTest m, Eq a, Show a) =>
  Iso' a b ->
  a ->
  m ()
isoToFrom i a =
  a ^. i ^. from i === a

prop_iso ::
  (Halves a b, Eq a, Show a) =>
  Gen a ->
  Property
prop_iso g =
  property $ forAll g >>= isoToFrom halves

prop_word16 ::
  Property
prop_word16 =
  prop_iso $ Gen.word16 Range.linearBounded

prop_word32 ::
  Property
prop_word32 =
  prop_iso $ Gen.word32 Range.linearBounded

prop_word64 ::
  Property
prop_word64 =
  prop_iso $ Gen.word64 Range.linearBounded

prop_int16 ::
  Property
prop_int16 =
  prop_iso $ Gen.int16 Range.linearBounded

prop_int32 ::
  Property
prop_int32 =
  prop_iso $ Gen.int32 Range.linearBounded

prop_int64 ::
  Property
prop_int64 =
  prop_iso $ Gen.int64 Range.linearBounded

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  result <- checkParallel $ Group "Props"
    [ ("prop_word16", prop_word16)
    , ("prop_word32", prop_word32)
    , ("prop_word64", prop_word64)
    , ("prop_int16",  prop_int16)
    , ("prop_int32",  prop_int32)
    , ("prop_int64",  prop_int64)
    ]
  unless result $ exitFailure
