module Main where

import           Parameters
import           Render
import           Simulation

import           Control.Monad       (when)
import           Data.IORef          (IORef, newIORef, writeIORef)
import           Graphics.UI.GLUT
import           System.Clock        (Clock (Monotonic), TimeSpec (..), getTime)
import           System.Clock.TimeIt
import           System.Exit         (exitSuccess)

main :: IO ()
main = do
    (_, _) <- getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, DoubleBuffered, WithSamplesPerPixel 4]
    initialWindowSize $= Size width height
    initialWindowPosition $= Position 100 100
    _ <- createWindow "Blueberry Goblins NeuroEvolution"
    s <- randomSimulation
    simulation <- newIORef s
    t <- getTime Monotonic
    time <- newIORef t
    displayCallback $= display simulation
    reshapeCallback $= Just reshape
    idleCallback $= Just (idle simulation time)
    lineWidth $= 1
    lineSmooth $= Enabled
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

idle :: IORef Simulation -> IORef TimeSpec -> IdleCallback
idle simulation time = do
    t2 <- getTime Monotonic
    t1 <- get time
    writeIORef time t2
    let dt = diffSeconds t2 t1
    sim <- get simulation
    sim' <- stepSimulation (realToFrac dt) sim
    writeIORef simulation sim'
    let newStep = step . stats $ sim'
    when (even newStep) $ postRedisplay Nothing

keyboardMouse :: IORef Simulation -> KeyboardMouseCallback
keyboardMouse _simulation key _state _modifiers _position = case key of
    (Char 'q') -> exitSuccess
    _          -> return ()
