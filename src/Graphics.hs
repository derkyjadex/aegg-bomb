{-# LANGUAGE DataKinds #-}

module Graphics where

import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Data.Vinyl
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import Graphics.GLUtil
import Graphics.GLUtil.Camera2D
import Graphics.VinylGL
import Linear (V2(..), V3(..), M33)
import System.IO
import Physics

data Scene = Scene [Box] [(String, Pos, Box)]

type Display = FieldRec '[ '("cam", M33 GLfloat) ]
type VertexCoord = '("vertexCoord", V2 GLfloat)
type VertexColor = '("vertexColor", V3 GLfloat)

vertexCoord :: SField VertexCoord
vertexCoord = SField

vertexColor :: SField VertexColor
vertexColor = SField

data RenderChan = RenderChan (MVar Scene)

newRenderChan :: IO RenderChan
newRenderChan =
  liftM RenderChan $ newMVar $ Scene [] []

sendScene :: RenderChan -> Scene -> IO ()
sendScene (RenderChan var) msg =
  swapMVar var msg >> return ()

readScene :: RenderChan -> IO Scene
readScene (RenderChan var) = readMVar var


toVertex :: Double -> Double -> GLfloat -> GLfloat -> GLfloat -> FieldRec [VertexCoord, VertexColor]
toVertex x y r g b =
  vertexCoord =: (V2 (realToFrac x) (realToFrac y)) <+>
  vertexColor =: (V3 r g b)

makeWallsVAO :: ShaderProgram -> [Box] -> IO VertexArrayObject
makeWallsVAO shader walls = do
  let is = take (6 * length walls) $
           concatMap (flip map [0, 1, 2, 2, 3, 0] . (+)) [0,4..]
      wallVertices ((minX, minY), (maxX, maxY)) =
           [toVertex minX minY 0.9 0.9 0.9
           ,toVertex maxX minY 0.9 0.9 0.9
           ,toVertex maxX maxY 0.9 0.9 0.9
           ,toVertex minX maxY 0.9 0.9 0.9
           ]
      vs = concatMap wallVertices walls

  indices <- bufferIndices is
  verts <- bufferVertices vs
  makeVAO $ do
    enableVertices' shader verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just indices

makePlayersVAO :: ShaderProgram -> [(String, Pos, Box)] -> IO VertexArrayObject
makePlayersVAO shader players = do
  let is = take (3 * length players) $
           concatMap (flip map [0, 1, 2] . (+)) [0,3..]
      playerVertices (_, (x, y), ((minX, minY), (maxX, maxY))) =
           [toVertex (x + minX) (y + minY) 0.0 1.0 0.0
           ,toVertex (x + maxX) (y + minY) 0.0 1.0 0.0
           ,toVertex (x + (minX + maxX) / 2) (y + maxY) 1.0 1.0 0.0
           ]
      vs = concatMap playerVertices players
  indices <- bufferIndices is
  verts <- bufferVertices vs
  makeVAO $ do
    enableVertices' shader verts
    bindVertices verts
    bindBuffer ElementArrayBuffer $= Just indices

makeRenderer :: IO (Display -> Scene -> IO ())
makeRenderer = do
  shader <- simpleShaderProgram "shader.vs" "shader.fs"
  return $ \display (Scene walls players) -> do
    currentProgram $= Just (program shader)
    setUniforms shader display

    wallsVAO <- makeWallsVAO shader walls
    withVAO wallsVAO $ drawIndexedTris $ fromIntegral $ 2 * length walls

    playersVAO <- makePlayersVAO shader players
    withVAO playersVAO $ drawIndexedTris $ fromIntegral $ length players

  where texSampler = SField :: SField '("tex", GLint)

setup :: IO (Display -> Scene -> IO ())
setup = do
  clearColor $= Color4 0.1 0.1 0.1 1
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  makeRenderer

keyCallback :: G.KeyCallback
keyCallback win key scancode action mods =
  if key == G.Key'Escape && action == G.KeyState'Pressed
  then G.setWindowShouldClose win True
  else return ()

renderMain :: RenderChan -> IO ()
renderMain chan = do
  withWindow 640 480 "Aegg Bomb" $ \w -> do
    G.setKeyCallback w $ Just keyCallback
    renderer <- setup
    mainLoop chan w renderer

mainLoop :: RenderChan -> G.Window -> (Display -> Scene -> IO ()) -> IO ()
mainLoop chan w renderer = do
  shouldClose <- G.windowShouldClose w
  when (not shouldClose) $ do
    (width, height) <- G.getFramebufferSize w
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]

    let cameraMatrix = camMatrix camera2D
        info = SField =: cameraMatrix
    scene <- readScene chan
    renderer info scene

    G.swapBuffers w
    G.pollEvents
    mainLoop chan w renderer

withWindow :: Int -> Int -> String -> (G.Window -> IO ()) -> IO ()
withWindow width height title f = do
  G.setErrorCallback $ Just errorCallback
  result <- G.init
  when result $ do

    G.windowHint $ G.WindowHint'ClientAPI G.ClientAPI'OpenGL
    G.windowHint $ G.WindowHint'OpenGLForwardCompat True
    G.windowHint $ G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
    G.windowHint $ G.WindowHint'ContextVersionMajor 3
    G.windowHint $ G.WindowHint'ContextVersionMinor 2

    m@(~(Just w)) <- G.createWindow width height title Nothing Nothing
    when (isNothing m) (error "Couldn't create window")

    G.makeContextCurrent m
    f w
    G.setErrorCallback $ Just errorCallback
    G.destroyWindow w
    G.terminate
  where errorCallback e s = hPutStrLn stderr s
