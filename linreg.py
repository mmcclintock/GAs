import numpy as np
import numpy.random as npr

# environment
points = np.matrix([[-1, 0, 3, 5],[-0.5, 0, 1.5, 2.5]], dtype=np.float64)
numPoints = points.shape[1]

# configuration
popSize = 100
numberGens = 10000
vecSize = 2
mutationRate = 0.01
recombinationRate = 0.5

# define the fitness function
def fitness(pop): 
  x = points[0,:]
  y = np.tile(points[1,:], (popSize,1))
  m = pop[:,0]
  c = np.tile(pop[:,1], (1,numPoints))
  return 1000/(1+np.square(y - m*x - c).sum(axis=1))

# generate a random initial population
pop = np.matrix(npr.uniform(-10, 10, popSize*vecSize).reshape(popSize, vecSize))

# start the evolution loop
n=0
while n<numberGens:
  fit = fitness(pop)
  prob = fps(fit)
  sel = selection(pop, prob)
  pop = mutate(sel)
  n += 1
