module Parameters where

simDuration, popSize, numFood, width, height, tournamentSize :: Num a => a
vMax, aMax, wMax, eatRadius, crossover, mutation, elitism :: Floating a => a
width = 600 -- m
height = 600 -- m
vMax = 200 -- m/s
aMax = 100 -- m/s^2
wMax = 360 * 10 -- deg
popSize = 200
numFood = 150
eatRadius = 5 -- m
simDuration = 10 -- s
crossover = 0.8 -- 0-1
mutation = 0.03 -- 0-1
elitism = 0.1 -- 0-1
tournamentSize = 10
