{-# LANGUAGE BangPatterns #-}
module Lib where

import Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.List
import           Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as VA
import qualified Data.Vector.Mutable as VM
import           Data.Function
import           Debug.Trace

-- |
-- prop> \p q -> (q > 0 && isPrime q && p > 0 && p < q) ==> (p * (p `inverseMod` q) `mod` q === 1)
inverseMod :: Integer -> Integer -> Integer
inverseMod 1 q = 1
inverseMod p q = (n * q + 1) `div` p
  where
    n = p - (q `mod` p) `inverseMod` p

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

babyStepGiant :: Integer -> Integer -> Integer -> Int -> Integer
babyStepGiant g h p m = go h 0
  where
    v :: Vector (Int, Integer)
    v = V.generate m (\i -> (i, g ^ i `mod` p))
    list =
      runST
        (do v' <- V.unsafeThaw v
            VA.sortBy (compare `on` snd) v'
            V.unsafeFreeze v')
    gInv = (g ^ m `mod` p) `inverseMod` p
    go :: Integer -> Int -> Integer
    go !y !i
      | i >= m = error "i >= m"
      | otherwise =
        traceShow (y, list) $
        case fst <$> (binarySearch snd y list) of
          Just j -> fromIntegral i * fromIntegral m + fromIntegral j
          Nothing -> go (y * gInv `mod` p) (i + 1)


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Numbers.Primes
