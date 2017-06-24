{-# LANGUAGE BangPatterns #-}
module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Foldable
import           Data.Function
import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import           Data.List
import           Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as VA
import qualified Data.Vector.Mutable as VM
import           Debug.Trace
import           GHC.Integer.GMP.Internals
import           GHC.Stack

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

buildLookupTable :: Integer -> Integer -> Int -> ST s (HashTable s Integer Int)
buildLookupTable g p m = do
  traceM ("Building lookup table of size: " ++ show m)
  table <- HT.newSized m
  mapM_
    (\i -> HT.insert table (powModInteger g (fromIntegral i) p) i)
    [0 .. m - 1]
  return table

babyStepGiant :: Integer -> Integer -> Integer -> Integer -> HashTable s Integer Int -> ST s Integer
babyStepGiant g h p m table = go h 0
  where
    gInv = powModInteger g m p `recipModInteger` p
    babySteps = (p `div` m) + 1
    go !y !i
      | i >= fromIntegral babySteps = error "i >= m"
      | otherwise = do
        val <- HT.lookup table y
        case val of
          Just j -> return (i * m + fromIntegral j)
          Nothing -> go (y * gInv `mod` p) (i + 1)

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

pohligHellman :: Integer -> Integer -> Integer -> [(Integer, Int)] -> Integer
pohligHellman g h p factors =
  sum'
    (zipWith
       (\ai mi ->
          let bi = gsize `div` mi
          in ai * bi * (bi `recipModInteger` mi) `mod` gsize)
       as
       ms) `mod`
  gsize
  where
    gsize = p - 1
    ms = map (uncurry (^)) factors
    as =
      map
        (\(pi, ei) ->
           let res = pohligHellman' g h p pi ei
           in traceShow (pi, ei, res) res)
        factors

pohligHellman' :: Integer -> Integer -> Integer -> Integer -> Int -> Integer
pohligHellman' g h p pi ei =
  runST $ do
    table <- buildLookupTable g0 p (fromIntegral m)
    l0 <- babyStepGiant g0 h0 p m table
    ls <- go table [l0]
    return
      (sum' (zipWith (\l i -> l * powModInteger pi i (pi ^ ei)) ls [0 ..]) `mod`
       (pi ^ ei))
  where
    m :: Integer
    m =
      if p > 10 ^ 12
        then 5 * 10 ^ 6
        else ceiling (sqrt (fromIntegral p))
    gsize = p - 1
    exponent = gsize `div` pi
    g0 = powModInteger g exponent p
    h0 = powModInteger h exponent p
    go table ls
      | length ls == ei = return ls
      | otherwise = do
        let hk = hk0 * hk1 `mod` p
              where
                hk0 = powModInteger h e p
                hk1 =
                  (powModInteger
                     (powModInteger g e p)
                     (sum' (zipWith (\l i -> l * pi ^ i) ls [0 ..]))
                     p) `recipModInteger`
                  p
                !e = gsize `div` (pi ^ (length ls + 1))
        lk <- babyStepGiant g0 hk p m table
        go table (ls ++ [lk])

p :: Integer
p =
  12812033272825343573241964822155518150863989454698536013844541575691016995569569178157355531014735958545538469803461721165865131357287826603167548085422432164852828763636499492253106094452179644999836997538091041625475400951445399367587477344297734846220209516778109053851098073160657134265949614899121092961731911201

pk :: Integer
pk =
  11214946957486257570931068175545968496744350981088326496998738223678449275341938500830930932856514631454393721163180060136363342367038564716782908459496412686082153401147924245851495623562748281729274527247449146795080120905454403270910973163616750436499747225020525711750786445759687172506314710559110766135479633753

factors :: [(Integer, Int)]
factors =
  [ (2, 5)
  , (3, 1)
  , (5, 2)
  , (784004352084511, 1)
  , (1027227490406197, 1)
  , (654902746474663, 1)
  , (1036939576553807, 1)
  , (596188499292077, 1)
  , (892468311744169, 1)
  , (659631355866083, 1)
  , (957233047034401, 1)
  , (724125034830403, 1)
  , (932208036802673, 1)
  , (884513720203747, 1)
  , (669103258616449, 1)
  , (706649413290787, 1)
  , (778270471074587, 1)
  , (1039970558488139, 1)
  , (582911323916071, 1)
  , (688320490973009, 1)
  , (1084631666707019, 1)
  , (650445618231881, 1)
  , (567579678437593, 1)
  , (791412759766441, 1)
  ]

g :: Integer
g = 7
