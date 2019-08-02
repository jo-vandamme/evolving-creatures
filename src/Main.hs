module Main where

import Parameters
import Simulation
import Render

import System.Exit (exitWith, ExitCode (ExitSuccess))
import Data.IORef (IORef, newIORef, writeIORef)
import System.Clock (TimeSpec (..), Clock (Monotonic), getTime) 
import System.Clock.TimeIt
import Text.Printf
import Graphics.UI.GLUT

main :: IO ()
main = do
    (_, _) <- getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, DoubleBuffered, WithSamplesPerPixel 4]
    initialWindowSize $= Size width height
    initialWindowPosition $= Position 100 100
    _ <- createWindow "Hungry Creatures NeuroEvolution"
    s <- randomSimulation
    simulation <- newIORef s
    t <- getTime Monotonic
    time <- newIORef t
    displayCallback $= display simulation
    reshapeCallback $= Just reshape
    idleCallback $= Just (idle simulation time)
    keyboardMouseCallback $= Just (keyboardMouse simulation)
    mainLoop

reshape :: Size -> IO ()
reshape s@(Size w h) = do
    matrixMode $= Projection
    loadIdentity
    ortho 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    viewport $= (Position 0 0, s)

display :: IORef Simulation -> DisplayCallback
display simulation = do
    s <- get simulation
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    renderSimulation s
    flush
    swapBuffers

{-
diffSec :: TimeSpec -> TimeSpec -> Double
diffSec (TimeSpec s1 ns1) (TimeSpec s2 ns2) = (fromIntegral dt) / 1e9
    where t1 = (toInteger s1) * 1000000000 + toInteger ns1
          t2 = (toInteger s2) * 1000000000 + toInteger ns2
          dt = t1 - t2
-}

idle :: IORef Simulation -> IORef TimeSpec -> IdleCallback
idle simulation time = do
    s <- get simulation
    t2 <- getTime Monotonic
    t1 <- get time
    writeIORef time t2 
    let dt = diffSeconds t2 t1
    putStrLn $ printf "[%d] - %.2fms" (step s) (dt * 1000)
    sim <- get simulation
    sim' <- stepSimulation (realToFrac dt) sim
    writeIORef simulation sim'
    postRedisplay Nothing

keyboardMouse :: IORef Simulation -> KeyboardMouseCallback
keyboardMouse _simulation key _state _modifiers _position = case key of
    (Char 'q') -> exitWith ExitSuccess
    _ -> return ()
