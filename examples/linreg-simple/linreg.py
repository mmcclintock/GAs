import numpy as np
import numpy.random as npr
import matplotlib.pyplot as plt
from GA import fps, creep_mutation, elitism

# environment
xs = np.linspace(0,10,500)
ys = 0.5*xs + npr.normal(0,0.3,500)
points = np.matrix([xs,ys])

# configuration
popSize = 100
numberGens = 300
vecSize = 2
mutationRate = 0.1
recombinationRate = 0.5

# define the fitness function
def fitness(pop): 
  x = points[0,:]
  y = np.tile(points[1,:], (pop.shape[0],1))
  m = pop[:,0]
  c = np.tile(pop[:,1], (1,points.shape[1]))
  return 1000/(1+np.square(y - m*x - c).sum(axis=1)/100)

 
# generate a random initial population
initPop = np.matrix(npr.uniform(-10, 10, popSize*vecSize).reshape(popSize, vecSize))


results = np.zeros(numberGens)  # array for results

def run(n=numberGens, pop=initPop):

  fit = fitness(pop) # calculate fitness of initial population

  # locate the best individual
  j = fit.argmax()
  elite = (pop[j,:], fit[j])

  for i in range(n):  # evolution loop
 
    # create new population which replaces the old one (generational survivor selection)
    pop = fps(pop, fit)                   # choose parents
    # combination(pop, recombinationRate)   # recombination
    creep_mutation(pop, mutationRate)     # mutation

    fit = fitness(pop)  # calculate fitness of the new population

    elite = elitism(pop, fit, elite)  # ensure the best indiv. survives (elitism)

    results[i] = elite[1]

  # plot fitness progression
  plt.plot(results,'b-')
  plt.savefig('fitness.png')
  plt.close()

  # plot the data points
  plt.plot(xs, ys, 'b.')

  # plot some solutions from the final population
  ix = npr.randint(0, pop.shape[0], 20)
  for i in range(ix.size):
    m = pop[ix[i],0]
    c = pop[ix[i],1]
    plt.plot(xs, m*xs+c, 'g-')

  # find the best solution and plot
  fit = fitness(pop)
  i = fit.argmax()
  m = pop[i,0]
  c = pop[i,1]
  print "Best solution: y= {0}*x + {1}, Fitness = {2}".format(m, c, fit[i])
  plt.plot(xs, m*xs+c, 'r-')  # best fit
  plt.savefig('linebestfit.png')
  print "Done!"

run()