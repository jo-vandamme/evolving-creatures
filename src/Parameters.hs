module Parameters where

popSize, numFood, width, height, numSteps, tournamentSize :: Num a => a
vMax, aMax, wMax, eatRadius, crossover, mutation, elitism :: Floating a => a
width = 700
height = 700
vMax = 150 
aMax = 15
wMax = 720
popSize = 200
numFood = 100
eatRadius = 10
numSteps = 1000
crossover = 0.8
mutation = 0.03
elitism = 0.1
tournamentSize = 5

