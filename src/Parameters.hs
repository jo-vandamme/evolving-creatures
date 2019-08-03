module Parameters where

popSize, numFood, width, height, numSteps, tournamentSize :: Num a => a
vMax, aMax, wMax, eatRadius, crossover, mutation, elitism :: Floating a => a
width = 500
height = 500
vMax = 100 
aMax = 10
wMax = 720
popSize = 100
numFood = 30
eatRadius = 10
numSteps = 2000
crossover = 0.8
mutation = 0.03
elitism = 0.1
tournamentSize = 5

