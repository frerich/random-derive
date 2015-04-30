random-derive
=============

This module exposes a `deriveRandom` function which automatically creates
`Random` instances for data types instantiating both `Enum` as well as
`Bounded`.

This is useful for getting random values (or sequences of random values) of
custom types, e.g.:

    {-# LANGUAGE TemplateHaskell #-}

    import System.Random (mkStdGen, randoms)
    import System.Random.TH

    data Color = Red | Green | Blue deriving (Enum, Bounded)

    $(deriveRandom ''Color)

    -- Yields an infinite sequence of random colors given some initial seed.
    randomColors :: Int -> [Color]
    randomColors seed = randoms (mkStdGen seed)

