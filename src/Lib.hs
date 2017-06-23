{-# LANGUAGE BangPatterns #-}
module Lib where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.List
import           Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as VA
import qualified Data.Vector.Mutable as VM
import           Debug.Trace
import           GHC.Integer.GMP.Internals
import           GHC.Stack

inverseMod :: Integer -> Integer -> Integer
inverseMod = recipModInteger

binarySearch :: Ord a => (b -> a) -> a -> Vector b -> Maybe b
binarySearch proj x v = go 0 (V.length v)
  where
    go !l !u
      | u <= l = Nothing
      | otherwise =
        case compare (proj (v ! k)) x of
          LT -> go (k + 1) u
          EQ -> Just (v ! k)
          GT -> go l k
      where
        k = (u + l) `shiftR` 1

buildLookupTable :: Integer -> Integer -> Int -> V.Vector (Int, Integer)
buildLookupTable g p m =
  trace ("Building lookup table of size: " ++ show m) $
  let v :: Vector (Int, Integer)
      v = V.generate m (\i -> (i, g ^ i `mod` p))
  in runST
       (do v' <- V.unsafeThaw v
           VA.sortBy (compare `on` snd) v'
           V.unsafeFreeze v')

babyStepGiant :: Integer -> Integer -> Integer -> Integer -> Vector (Int, Integer) -> Integer
babyStepGiant g h p m table = traceShow (g,h,p,m) $ go h 0
  where
    gInv = (g ^ m `mod` p) `inverseMod` p
    babySteps = (p `div` m) + 1
    go :: Integer -> Integer -> Integer
    go !y !i
      | i >= fromIntegral babySteps = error "i >= m"
      | otherwise =
        case fst <$> (binarySearch snd y table) of
          Just j -> fromIntegral i * fromIntegral m + fromIntegral j
          Nothing -> go (y * gInv `mod` p) (i + 1)

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

pohligHellman :: Integer -> Integer -> Integer -> [(Integer, Int)] -> Integer
pohligHellman g h p factors =
  sum'
    (zipWith
       (\ai mi ->
          let bi = gsize `div` mi
          in ai * bi * (bi `inverseMod` mi) `mod` gsize)
       as
       ms) `mod`
  gsize
  where
    gsize = p - 1
    ms = map (uncurry (^)) factors
    as = map (\(pi, ei) -> pohligHellman' g h p pi ei) factors

pohligHellman' :: Integer -> Integer -> Integer -> Integer -> Int -> Integer
pohligHellman' g h p pi ei =
  sum' (zipWith (\l i -> l * powModInteger pi i (pi ^ ei)) ls [0 ..]) `mod`
  (pi ^ ei)
  where
    m :: Integer
    m =
      if p > 10 ^ 12
        then 10 * 10 ^ 6
        else ceiling (sqrt (fromIntegral p))
    table = buildLookupTable g0 p (fromIntegral m)
    gsize = p - 1
    exponent = gsize `div` pi
    g0 = powModInteger g exponent p
    h0 = powModInteger h exponent p
    l0 = babyStepGiant g0 h0 p m table
    ls = l0 : map (\i -> go (take i ls)) [1 .. ei - 1]
    go ls = babyStepGiant g0 hk p m table
      where
        k = length ls
        !hk = hk0 * hk1 `mod` p
          where
            !hk0 = powModInteger h e p
            !hk1 =
              (powModInteger
                 (powModInteger g e p)
                 (sum' (zipWith (\l i -> l * pi ^ i) ls [0 ..]))
                 p) `inverseMod`
              p
            !e = gsize `div` (pi ^ (k + 1))
