import numpy as np
import numpy.random as npr

# fitness proportional selection using stochastic universal sampling (roulette wheel)
def fps(pop, fit):
  popSize = pop.shape[0]
  regions = fit.cumsum() / fit.sum()  # normalize and accumulate fitness values to create regions on the wheel
  arms = np.arange(popSize, dtype=np.float64) / popSize  # create n=popSize evenly spaced arms
  spin = npr.uniform(0.0, 1.0/popSize, 1) + arms  # spinning the wheel is equivalent to randomly selecting an offset between (0,1/n)
  f = np.vectorize(lambda a: (regions <= a).sum()) # find the region number for each arm
  return pop[f(spin),:]

# mutate some genes by adding a random number from a gaussian distribution
def creep_mutation(pop, mr, std=1, mean=0):
  rands = np.matrix(npr.uniform(size=pop.size).reshape(pop.shape)) < mr
  pop[rands] = np.array(pop[rands])[0] + npr.normal(mean, std, rands.sum())

# ensure the best indiv. survives
def elitism(pop, fit, elite):
  i = fit.argmax()
  if fit[i] < elite[1]:
    pop[0], fit[0] = elite
    return elite
  return (pop[i,:], fit[i])