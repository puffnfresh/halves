module Data.Halves.Tuple (
  tuple4
, tuple8
) where

import           Control.Lens

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
