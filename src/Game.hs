module Game where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Calendar
import Physics
import Graphics

data GameState = GameState { gameRunning :: Bool
                           , walls :: [Box]
                           , players :: [Player]
                           , eggs :: [Egg]
                           }

data Player = Player { playerName :: String
                     , playerChan :: Chan PlayerMsg
                     , playerNextTrace :: UTCTime
                     , playerPos :: Pos
                     , playerVel :: Vec
                     , playerBounds :: Box
                     }
              deriving (Eq)

data Egg = Egg { eggPlayer :: String
               , eggPos :: Pos
               , eggVel :: Vec
               , eggHeight :: Double
               , eggVVel :: Double
               , eggBounds :: Box
               }

data GameMsg = Frame
             | Stop
             | AddPlayer String (Chan PlayerMsg)
             | MovePlayer String Vec
             | RemovePlayer String
             | ThrowEgg String Vec
             deriving (Eq)

type GameChan = Chan GameMsg

data PlayerMsg = CurrentPos Pos
               | PlayersSeen [(String, Pos)]
               | EggsSeen [Pos]
               | Removed
               deriving (Eq)

data PlayerCmd = Move Vec
               | Throw Vec
               deriving (Show, Read)

newGameChan :: IO GameChan
newGameChan = newChan

newGame :: [Box] -> GameState
newGame ws = GameState { gameRunning = True
                       , walls = ws
                       , players = []
                       , eggs = []
                       }

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

processInput :: GameMsg -> GameState -> GameState
processInput Frame game = game
processInput Stop game = game { gameRunning = False }

processInput (AddPlayer name chan) game =
  let player = Player { playerName = name
                      , playerChan = chan
                      , playerNextTrace = zeroTime
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

processInput (ThrowEgg owner vel) game =
  fromMaybe game $ do
    player <- find ((owner ==) . playerName) $ players game
    let egg = Egg { eggPlayer = owner
                  , eggPos = playerPos player
                  , eggVel = vel
                  , eggHeight = 0
                  , eggVVel = 1
                  , eggBounds = ((-0.25, -0.25), (0.25, 0.25))
                  }
    return $ game { eggs = egg : eggs game }

runInput :: GameChan -> GameState -> IO GameState
runInput chan game = do
  msg <- readChan chan
  case msg of
    Frame -> return game
    _ -> runInput chan $ processInput msg game

runPhysics :: GameState -> GameState
runPhysics game =
  let players' = map updatePlayer $ players game
      eggs' = map updateEgg $ eggs game
  in game { players = players'
          , eggs = eggs'
          }
  where updatePlayer p =
          let pos = playerPos p
              vel = playerVel p
              bounds = playerBounds p
              otherPlayers = filter (/= p) $ players game
              playersBoxes = map (boxAt <$> playerBounds <*> playerPos) otherPlayers
              boxes = playersBoxes ++ walls game
              Trace pos' _ = trace pos bounds vel boxes
          in p { playerPos = pos' }
        updateEgg e =
          let pos = eggPos e
              vel = eggVel e
              bounds = eggBounds e
              playerBoxes = map (boxAt <$> playerBounds <*> playerPos) $ players game
              boxes = playerBoxes ++ walls game
              Trace pos' _ = trace pos bounds vel boxes
              vVel' = eggVVel e - 0.1
              height' = eggHeight e + vVel'
          in e { eggPos = pos'
               , eggVVel = vVel'
               , eggHeight = height'
               }

runRender :: GameState -> RenderChan -> IO ()
runRender game chan =
  let ws = walls game
      ps = map ((,,) <$> playerName <*> playerPos <*> playerBounds) $ players game
      es = map ((,) <$> eggPos <*> eggBounds) $ eggs game
  in sendScene chan $ Scene ws ps es

calculatePlayersSeen :: GameState -> Player -> [(String, Pos)]
calculatePlayersSeen game player =
  let otherPlayers = filter (/= player) $ players game
      candidates = map ((,) <$> playerName <*> playerPos) otherPlayers
      pos = playerPos player
      ws = walls game
  in filter (\(_, pos') -> canSee pos' pos ws) candidates

calculateEggsSeen :: GameState -> Player -> [Pos]
calculateEggsSeen game player =
  let candidates = map (eggPos) $ eggs game
      pos = playerPos player
      ws = walls game
  in filter (\pos' -> canSee pos' pos ws) candidates

runTrace :: GameState -> UTCTime -> IO GameState
runTrace game t =
  if not $ gameRunning game
  then do
    send (const Removed) $ players game
    return game
  else let ps = filter needsUpdate $ players game
           nextTrace = addUTCTime playerTraceTime t
           players' = map (\p -> if needsUpdate p
                                 then p { playerNextTrace = nextTrace }
                                 else p)
                          (players game)
       in do
         send (CurrentPos . playerPos) ps
         send (PlayersSeen . calculatePlayersSeen game) ps
         send (EggsSeen . calculateEggsSeen game) ps
         return game { players = players' }
  where send f ps = mapM_ (\p -> writeChan (playerChan p) (f p)) ps
        needsUpdate p = playerNextTrace p <= t

targetFrameTime :: NominalDiffTime
targetFrameTime = 1 / 60

playerTraceTime :: NominalDiffTime
playerTraceTime = 1 / 2

delayUntil :: UTCTime -> IO ()
delayUntil end = do
  start <- getCurrentTime
  let delay = diffUTCTime end start
  threadDelay $ round $ delay * 1000000

runGame :: GameChan -> RenderChan -> GameState -> IO ()
runGame chan renderChan game = do
  t0 <- getCurrentTime
  runGame' chan renderChan game t0

runGame' :: GameChan -> RenderChan -> GameState -> UTCTime -> IO ()
runGame' chan renderChan game t0 = do
  writeChan chan Frame
  game' <- runPhysics <$> runInput chan game
  runRender game' renderChan
  game'' <- runTrace game' t0
  let t1 = addUTCTime targetFrameTime t0
  delayUntil t1
  when (gameRunning game'') $ runGame' chan renderChan game'' t1
