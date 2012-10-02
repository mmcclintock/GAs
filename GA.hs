{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module GA
( Genome (..)
, GAConfig (..)
, randomPopulation
, fps
, selection
, mutationCreep
, GAStack
, asks
, runReaderT
, withSystemRandom
, getBest
) where 

import BasicPrelude as P
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.Reader
-- import Data.Sequence as S

class (Unbox e, Variate e) => Genome e where
  range :: (e, e)
  
data GAConfig = GAConfig 
  { popSize :: Int
  , numberGens :: Int
  , vecSize :: Int
  , mutationRate :: Double
  , recombinationRate :: Double
  , randomGenerator :: GenIO
  }

type GAStack = ReaderT GAConfig IO

randomPopulation :: Genome e => GAStack (Array U DIM2 e)
randomPopulation = do
  vs <- asks vecSize
  ps <- asks popSize
  rg <- asks randomGenerator
  rs <- liftIO $ U.replicateM (vs*ps) (uniformR range rg)
  return $! fromUnboxed (Z :. ps :. vs) rs

-- fitness proportional selection
fps :: Array U DIM1 Double -> Array D DIM1 Double
fps fs = R.map (/s) fs
  where s = sumS fs R.! Z

  
-- stochastic universal sampling. Roulette wheel with equaly spaced arms
sus :: Double -> Array U DIM1 Double -> U.Vector Int
sus ro ps = U.map (\arm -> U.length $ U.filter (<=arm) wheel) arms
  where
    (Z :. u) = extent ps
    v = 1 / fromIntegral u
    arms = U.map (v*ro+) $ U.enumFromStepN 0 v u
    wheel = U.scanl1' (+) $ toUnboxed ps

-- parent selection
selection :: Genome e => Array U DIM2 e -> Array U DIM1 Double -> GAStack (Array D DIM2 e)
selection pop ps= do
  rg <- asks randomGenerator
  rs <- liftIO $ uniform rg
  let tr get (Z :. i :. p) = get (Z :. (sus rs ps U.! i) :. p)
  return $! traverse pop id tr 



-- whole arithmetic recombination
-- waRecombination :: Genome e => Array U DIM2 e -> GAStack (Array D DIM2 e)
-- waRecombination pop = do
--   rr <- asks recombinationRate
--   rg <- asks randomGenerator
--   return $! delay pop
-- 

-- shufflePop :: Genome e => Array U DIM2 e -> GAStack (Array D DIM2 e)
-- shufflePop pop = do
--   let (Z :. i :. _) = extent pop
--   rg <- asks randomGenerator
--   ris <- liftIO $ U.mapM (\u -> uniformR (0,u) rg) (U.enumFromStepN (i-1) (-1) i)
--   let xs = shuffle (fromList [1..i-1]) ris
--   let tr get (Z :. j :. p) = get (Z :. S.index xs j :. p)
--   return $! traverse pop id tr
-- 
-- 
-- shuffle :: Seq a -> UVector Int -> Seq a
-- shuffle xs rs = snd $ U.foldl' acc (xs, S.empty) rs
--   where acc (si, so) i = let (nsi, a) = extractSeq si i in (nsi, a<|so)
-- 
-- extractSeq :: Seq a -> Int -> (Seq a, a)
-- extractSeq s i = (a >< c, b)
--   where
--     (a, r) = S.splitAt i s
--     (b :< c) = viewl r
--  
-- combine :: Genome e => Double -> GenIO -> Int -> (UVector e, UVector e) -> GAStack (UVector e, UVector e)
-- combine rr gen vs (p1, p2) = do
--   r <- liftIO $ uniform gen
--   if r < rr 
--     then do
--       cp <- liftIO $ uniformR (0, vs) gen
--       let (h1, t1) = U.splitAt cp p1
--       let (h2, t2) = U.splitAt cp p2
--       return (h1 U.++ t2, h2 U.++ t1)
--     else
--       return (p1, p2)
-- 
-- recombination :: Genome e => Vector (UVector e) -> GAStack (Vector (UVector e))
-- recombination par = do
--   rr <- asks recombinationRate
--   gen <- asks randomGenerator
--   vs <- asks vecSize
-- 
--   let (mothers, fathers) = V.splitAt (V.length par `div` 2) par
--   offspring <- V.mapM (combine rr gen vs) $ V.zip mothers fathers
--   let (f, b) = V.unzip offspring
--   if odd $ V.length par
--     then return $! V.last par `V.cons` f V.++ b
--     else return $! f V.++ b

-- 
-- mutateReal :: Int -> Double -> GenIO -> UVector Double -> GAStack (UVector Double)
-- mutateReal size mr gen vals = do
--     bools <- U.replicateM size $ fmap (< mr) (liftIO (uniform gen))
--     creep <- U.replicateM size $ liftIO $ uniformR (-50.0,50.0) gen
--     return $! U.map mut $ U.zip3 bools creep vals
--   where
--     mut (True, creep, val) = val + creep
--     mut (False, _   , val) = val
-- 
-- mutationReal :: Vector (UVector Double) -> GAStack (Vector (UVector Double))
-- mutationReal pop = do
--   size <- asks vecSize
--   gen <- asks randomGenerator
--   mr <- asks mutationRate
--   V.mapM (mutateReal size mr gen) pop

mutationCreep :: Array U DIM2 Double -> GAStack (Array D DIM2 Double)
mutationCreep pop = do
    mr <- asks mutationRate
    rg <- asks randomGenerator
    let psh = extent pop
    rs <- fromUnboxed psh <$> U.replicateM (size psh) (liftIO $ uniform rg)
    cs <- fromUnboxed psh <$> U.replicateM (size psh) (liftIO $ standard rg)
    return $! traverse3 rs cs pop (\i _ _ -> i) (tr mr)
  where
    tr !mr g1 g2 g3 sh
      | g1 sh < mr = g2 sh + g3 sh
      | otherwise = g3 sh
    {-# INLINE tr #-}

 
getBest :: Genome e => Array U DIM2 e -> Array U DIM1 Double -> Double
getBest pop fit = fit ! (Z :. i)
  where i = U.maxIndex (toUnboxed fit)
-- 
-- elitism :: Genome e => UVector Fitness -> Vector (UVector e) -> Vector (UVector e) -> GAStack (Vector (UVector e))
-- elitism fit pop off
--   | (V.maximum . V.map fitness $ off) > bestParFit = return off
--   | otherwise = return . V.cons bestPar . V.tail $ off
--   where
--     cmp (f1,_) (f2,_) = compare f1 f2
--     (bestParFit, bestPar) = V.maximumBy cmp $ V.zip (V.convert fit) pop
