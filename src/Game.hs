module Game where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Function (fix)
import Data.Time
import Physics
import Graphics

data GameState = GameState { gameRunning :: Bool
                           , players :: [Player]
                           , walls :: [Box]
                           }

data Player = Player { playerName :: String
                     , playerChan :: Chan PlayerMsg
                     , playerPos :: Pos
                     , playerVel :: Vec
                     , playerBounds :: Box
                     }
              deriving (Eq)

data GameMsg = Frame
             | Stop
             | AddPlayer String (Chan PlayerMsg)
             | MovePlayer String Vec
             | RemovePlayer String
             deriving (Eq)

data PlayerMsg = NothingSeen Pos
               | Removed
               deriving (Eq)

data PlayerCmd = Move Vec
               deriving (Show, Read)

newGame :: GameState
newGame = GameState { gameRunning = True
                    , players = []
                    , walls = [ ((-11, -11), (-10, 10))
                              , ((-11, 10), (10, 11))
                              , ((10, -10), (11, 11))
                              , ((-10, -11), (11, -10))
                              ]
                    }

processInput :: GameMsg -> GameState -> GameState
processInput Frame game = game
processInput Stop game = game { gameRunning = False }

processInput (AddPlayer name chan) game =
  let player = Player { playerName = name
                      , playerChan = chan
                      , playerPos = (0, 0)
                      , playerVel = (0, 0)
                      , playerBounds = ((-0.5, -0.5), (0.5, 0.5))
                      }
  in game { players = player : players game }

processInput (MovePlayer name vel) game =
  let players' = map (\p -> if playerName p == name
                            then updatePlayer p
                            else p) $ players game
  in game { players = players' }
  where updatePlayer p =
          p { playerVel = vel }

processInput (RemovePlayer name) game =
  let players' = filter ((name /=) . playerName) $ players game
  in game { players = players' }

runInput :: Chan GameMsg -> GameState -> IO GameState
runInput chan game = do
  msg <- readChan chan
  case msg of
    Frame -> return game
    _ -> runInput chan $ processInput msg game

runPhysics :: GameState -> GameState
runPhysics game =
  let players' = map updatePos $ players game
  in game { players = players' }
  where updatePos p =
          let pos = playerPos p
              vel = playerVel p
              bounds = playerBounds p
              playersBoxes = map (boxAt <$> playerBounds <*> playerPos) $
                             filter (/= p) $
                             players game
              boxes = playersBoxes ++ walls game
              Trace pos' _ = trace pos bounds vel boxes
          in p { playerPos = pos' }

runRender :: GameState -> RenderChan -> IO ()
runRender game chan =
  let ws = walls game
      ps = map (liftA3 (,,) playerName playerPos playerBounds) $ players game
  in sendScene chan $ Scene ws ps

runTrace :: GameState -> IO ()
runTrace game =
  if not $ gameRunning game
  then mapM_ (\p -> writeChan (playerChan p) Removed) $ players game
  else return ()

targetFrameTime :: NominalDiffTime
targetFrameTime = 1 / 60

delayUntil :: UTCTime -> IO ()
delayUntil end = do
  start <- getCurrentTime
  let delay = diffUTCTime end start
  threadDelay $ round $ delay * 1000000

runGame :: Chan GameMsg -> RenderChan -> GameState -> IO ()
runGame chan renderChan game = do
  t0 <- getCurrentTime
  runGame' chan renderChan game t0

runGame' :: Chan GameMsg -> RenderChan -> GameState -> UTCTime -> IO ()
runGame' chan renderChan game t0 = do
  writeChan chan Frame
  game' <- liftM runPhysics $ runInput chan game
  runRender game' renderChan
  runTrace game'
  let t1 = addUTCTime targetFrameTime t0
  delayUntil t1
  when (gameRunning game) $ runGame' chan renderChan game' t1
