{-|
Module:         System.Random.TH
Copyright:      (c) 2015 Frerich Raabe
License:        BSD3
Maintainer:     frerich.raabe@gmail.com
Stability:      experimental

This module exposes a 'deriveRandom' function which automatically creates
'System.Random.Random' instances for data types instantiating both 'Enum' as well
as 'Bounded'.

This is useful for getting random values (or sequences of random values)
of custom types, e.g.:

> {-# LANGUAGE TemplateHaskell #-}
>
> import System.Random (mkStdGen, randoms)
> import System.Random.TH
>
> data Color = Red | Green | Blue deriving (Enum, Bounded)
>
> $(deriveRandom ''Color)
>
> -- Yields an infinite sequence of random colors given some initial seed.
> randomColors :: Int -> [Color]
> randomColors seed = randoms (mkStdGen seed)
-}

{-# LANGUAGE TemplateHaskell #-}

module System.Random.TH
    ( deriveRandom )
    where

import Language.Haskell.TH

import System.Random

-- |The 'deriveRandom' function derives a Random instance for the given type.
deriveRandom :: Name    --The type to generate a 'System.Random.Random' instance for; must inhibit both 'Enum' as well as 'Bounded'.
             -> Q [Dec]
deriveRandom n = [d|
    instance Random $(conT n) where
      randomR (lo, hi) g = (toEnum a, g')
        where
          (a, g') = randomR (fromEnum lo, fromEnum hi) g

      random = randomR (minBound, maxBound)
    |]
