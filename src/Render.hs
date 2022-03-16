module Render where

import           Food
import qualified GA
import           Organism
import           OrganismDNA
import           Parameters
import           Simulation

import           Graphics.UI.GLUT
import           Text.Printf

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
        color3f 0.3 0.5 1
        translate $ uncurry Vector3 (foodPos f) (0 :: GLfloat)
        scale s s (s :: GLfloat)
        renderPrimitive TriangleFan $
            mapM_ (uncurry vertex2f) (unitCircle 12)

renderOrganism :: Float -> OrganismDNA -> IO ()
renderOrganism best (OrganismDNA o) = do
    let f = health o / (2 * best) + 0.5 -- 0.5 to 1
        sz = 8 * f
    preservingMatrix $ do
        translate $ uncurry Vector3 (orgPos o) (0 :: GLfloat)
        scale sz sz (sz :: GLfloat)
        color3f 0.5 0.5 0.5
        renderPrimitive TriangleFan $
            mapM_ (uncurry vertex2f) (unitCircle 12)
        preservingMatrix $ do
            color3f 1 1 1
            rotate (heading o) $ Vector3 0 0 1
            scale 0.5 0.5 (0.5 :: GLfloat)
            translate $ Vector3 1.5 0 (0 :: GLfloat)
            renderPrimitive TriangleFan $
                mapM_ (uncurry vertex2f) (unitCircle 12)

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
    let genStr = printf "gen %d - %.0f FPS - %.1fs" (generation s) (meanFps s) (duration s)
        scoreStr = printf "best score %.0f - mean %.1f" (bestScore s) (meanScore s)
        lineH = 15
    drawRect (0.2, 0.2, 0.2) (0, height) (230, height - lineH * 3 - 5)
    drawText (1, 1, 1) (12, height - lineH - 8) genStr
    drawText (1, 1, 1) (12, height - lineH * 2 - 8) scoreStr

renderSimulation :: Simulation -> IO ()
renderSimulation s = do
    let getOrgs (GA.Population _ orgs) = orgs
        best = bestScore . stats $ s
    mapM_ renderFood $ foodParticles s
    mapM_ (renderOrganism best) $ getOrgs . organisms $ s
    renderStats . stats $ s
