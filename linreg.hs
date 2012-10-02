{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude as P
import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as U
import GA

-- the environment for linear regression is a set of x,y points,
points :: Array U DIM2 Double
points = fromListUnboxed (Z :. 2 :. 4) [-1.0, 0.0, 3.0, 5.0, 
                                        -0.5, 0.0, 1.5, 2.5]
-- these are all points on the line y=0.5x 
-- so the perfect solution is m=0.5, y=0.0


-- we need a way to judge individuals
fitness :: Array U DIM2 Double -> Array D DIM1 Double
fitness pop = R.map (\e -> 1000 / (1 + e)) sse
  where 
    sh (Z :. i :. _) (Z :. _ :. j) = Z :. i :. j
    tr g1 g2 (Z :. i :. j) = (y - m*x - c)^(2 :: Int)
      where
        m = g1 (Z :. i :. 0)
        c = g1 (Z :. i :. 1)
        x = g2 (Z :. 0 :. j)
        y = g2 (Z :. 1 :. j)
    {-# INLINE tr #-}
    sse = sumS $ traverse2 pop points sh tr


instance Genome Double where
  range = (-10.0, 10.0)

ga = do
  n <- asks numberGens
  finalPop <- foldl' (>>=) randomPopulation (replicate n evolve)
  let fit = computeS $ fitness finalPop
  return $! getBest finalPop fit
  
evolve pop = do
  fit <- computeP $ fitness pop
  probs <- computeP $ fps fit
  pars <- computeS <$> selection pop probs
  computeS <$> mutationCreep pars

conf rg = GAConfig { popSize = 20000
                   , numberGens = 5
                   , vecSize = 2
                   , mutationRate = 0.01
                   , recombinationRate = 0.5
                   , randomGenerator = rg
                   }

-- run the GA with a random seed
main = withSystemRandom (runReaderT ga . conf)
