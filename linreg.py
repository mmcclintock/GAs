import numpy as np
import numpy.random as npr
import random

# environment
points = np.matrix([[-1, 0, 3, 5],[-0.5, 0, 1.5, 2.5]], dtype=np.float64)
numPoints = points.shape[1]

# configuration
popSize = 200
numberGens = 1000
vecSize = 2
mutationRate = 0.1
recombinationRate = 0.5

# define the fitness function
def fitness(pop): 
  x = points[0,:]
  y = np.tile(points[1,:], (popSize,1))
  m = pop[:,0]
  c = np.tile(pop[:,1], (1,numPoints))
  return 1000/(1+np.square(y - m*x - c).sum(axis=1))


# fitness proportional selection using stochastic universal sampling
def fps(pop, fit):
  regions = fit.cumsum() / fit.sum()  # normalize and accumulate fitness values to create regions on the wheel
  arms = np.arange(popSize, dtype=np.float64) / popSize  # create n=popSize evenly spaced arms
  spin = npr.uniform(0.0, 1.0/popSize, 1) + arms  # spinning the wheel is equivalent to randomly selecting an offset between (0,1/n)
  f = np.vectorize(lambda a: (regions <= a).sum())
  return pop[f(spin),:]


def creep_mutation(pop):
  rands = np.matrix(npr.uniform(size=popSize*vecSize).reshape(popSize,vecSize)) < mutationRate 
  pop[rands] = np.array(pop[rands])[0] + npr.standard_normal(rands.sum())
     
# generate a random initial population
initPop = np.matrix(npr.uniform(-10, 10, popSize*vecSize).reshape(popSize, vecSize))

def run(n=numberGens, pop=initPop):
  for _ in range(n):  # evolution loop
    fit = fitness(pop)
    pop = fps(pop, fit)
    creep_mutation(pop)
  return pop[fitness(pop).argmax()]

print run()
