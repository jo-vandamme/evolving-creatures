module Math where

import Control.Monad.Random

type Vec2 = (Float, Float)

radians :: Float -> Float
radians th = th * pi / 180

degrees :: Float -> Float
degrees th = th * 180 / pi

vnorm :: Vec2 -> Float
vnorm (a, b) = sqrt $ a*a + b*b

vdiff :: Vec2 -> Vec2 -> Vec2
vdiff (a, b) (c, d) = (a - c, b - d)

vdist :: Vec2 -> Vec2 -> Float
vdist x y = vnorm $ vdiff x y

vdot :: Vec2 -> Vec2 -> Float
vdot (a, b) (c, d) = a * c + b * d

uniform :: (MonadRandom r, Floating a, Random a) => r a
uniform = getRandomR (-1, 1)

gauss :: (MonadRandom r, Floating a, Random a) => a -> r a
gauss stdev = do
    x1 <- getRandom
    x2 <- getRandom
    return $ stdev * sqrt (-2 * log x1) * cos (2 * pi * x2)
