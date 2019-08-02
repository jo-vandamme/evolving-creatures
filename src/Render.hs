module Render where

import Food
import Organism
import Simulation

import Graphics.UI.GLUT

unitCircle :: Int -> [(GLfloat, GLfloat)]
unitCircle n' = [(sin (2*pi*k/n), cos (2*pi*k/n)) | k <- [1..n]]
    where n = fromIntegral n'

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex $ Vertex3 x y 0

renderFood :: Food -> IO ()
renderFood f = do
    let s = 5
    preservingMatrix $ do
        color3f 1 0 1
        translate $ Vector3 (fst . foodPos $ f) (snd . foodPos $ f) (0 :: GLfloat)
        scale s s (s :: GLfloat)
        renderPrimitive TriangleFan $
            mapM_ (\(x, y) -> vertex2f x y) (unitCircle 12)

renderOrganism :: Organism -> IO ()
renderOrganism o = do
    let s = 5
    preservingMatrix $ do
        color3f 0 1 0
        translate $ Vector3 (fst . orgPos $ o) (snd . orgPos $ o) (0 :: GLfloat)
        scale s s (s :: GLfloat)
        rotate (heading o) $ Vector3 0 0 1
        renderPrimitive Triangles $ do
            vertex2f (-1) (-1.5)
            vertex2f 1 (-1.5)
            vertex2f 0 1.5

renderSimulation :: Simulation -> IO ()
renderSimulation s = do
    mapM_ renderFood $ foodParticles s
    mapM_ renderOrganism $ organisms s
