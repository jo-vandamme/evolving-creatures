module Render where

import Food
import Organism
import OrganismDNA
import qualified GA as GA
import Simulation
import Parameters

import Text.Printf
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
    let s = 3
    preservingMatrix $ do
        color3f 1 0 1
        translate $ Vector3 (fst . foodPos $ f) (snd . foodPos $ f) (0 :: GLfloat)
        scale s s (s :: GLfloat)
        renderPrimitive TriangleFan $
            mapM_ (\(x, y) -> vertex2f x y) (unitCircle 12)

renderOrganism :: Float -> OrganismDNA -> IO ()
renderOrganism best (OrganismDNA o) = do
    let s = 5
        b = 3
        c = (health o + b) / (best + b)
    preservingMatrix $ do
        color3f 0 c 0
        translate $ Vector3 (fst . orgPos $ o) (snd . orgPos $ o) (0 :: GLfloat)
        scale s s (s :: GLfloat)
        rotate (heading o) $ Vector3 0 0 1
        renderPrimitive Triangles $ do
            vertex2f (-1.5) 1
            vertex2f (-1.5) (-1)
            vertex2f 1.5 0

drawRect :: (Float, Float, Float) -> (Float, Float) -> (Float, Float) -> IO ()
drawRect (r, g, b) (x1, y1) (x2, y2) = do
    color3f r g b
    renderPrimitive TriangleStrip $ do
        vertex2f x1 y1
        vertex2f x2 y1
        vertex2f x2 y2
        vertex2f x1 y2
        vertex2f x1 y1

drawText :: (Float, Float, Float) -> (Float, Float) -> String -> IO ()
drawText (r, g, b) (x, y) s = do
    color3f r g b
    currentRasterPosition $= Vertex4 x y 0 1
    renderString Fixed8By13 s

renderStats :: Stats -> IO ()
renderStats s = do
    let nSteps = printf "step %d" (step s)
        fps = printf "%.0f" (meanFps s)
        genStr = "generation: " ++ (show . generation $ s)
        scoreStr = printf "best score: %.0f" (bestScore s)
        meanScoreStr = printf "mean score: %.0f" (meanScore s)
        avgFpsStr = "Avg: " ++ fps ++ " FPS"
        lineH = 15
    drawRect (0.2, 0.2, 0.2) (0, height) (140, height - lineH * 6 - 5)
    drawText (1, 1, 1) (12, height - lineH * 1 - 8) avgFpsStr 
    drawText (1, 1, 1) (12, height - lineH * 2 - 8) genStr 
    drawText (1, 1, 1) (12, height - lineH * 3 - 8) nSteps
    drawText (1, 1, 1) (12, height - lineH * 4 - 8) scoreStr
    drawText (1, 1, 1) (12, height - lineH * 5 - 8) meanScoreStr

renderSimulation :: Simulation -> IO ()
renderSimulation s = do
    let getOrgs (GA.Population _ orgs) = orgs
        best = bestScore . stats $ s
    mapM_ renderFood $ foodParticles s
    mapM_ (renderOrganism best) $ getOrgs . organisms $ s
    renderStats . stats $ s
