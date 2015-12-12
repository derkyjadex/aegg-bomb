module Game
       (newGameChan, runGame, newGame, GameMsg(..), PlayerMsg(..),
        PlayerCmd(..), GameChan)
       where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Time
import           Graphics
import           Physics
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type ObjectId = Int

data GameState =
  GameState {gameRunning :: Bool
            ,walls       :: [Box]
            ,objects     :: Map ObjectId GameObject
            ,traceQueue  :: [(UTCTime,ObjectId)]}

data GameObject
  = PlayerObj Player
  | EggObj Egg
  | ExplosionObj Explosion

data Player =
  Player {playerName      :: String
         ,playerChan      :: Chan PlayerMsg
         ,playerPos       :: Pos
         ,playerVel       :: Vec
         ,playerBounds    :: Box
         ,playerHealth    :: Double}
  deriving (Eq)

data Egg =
  Egg {eggPlayer :: String
      ,eggPos    :: Pos
      ,eggVel    :: Vec
      ,eggHeight :: Double
      ,eggVVel   :: Double
      ,eggBounds :: Box}

data Explosion =
  Explosion {explosionPos    :: Pos
            ,explosionT      :: Double
            ,explosionBounds :: Box}

data GameMsg
  = Frame
  | Stop
  | AddPlayer String
              (Chan PlayerMsg)
  | MovePlayer String
               Vec
  | RemovePlayer String
  | ThrowEgg String
             Vec
  deriving (Eq)

type GameChan = Chan GameMsg

data PlayerMsg
  = CurrentPos Pos
  | Health Double
  | PlayersSeen [(String,Pos)]
  | EggsSeen [Pos]
  | Removed
  deriving (Eq)

data PlayerCmd
  = Move Vec
  | Throw Vec
  deriving (Show,Read)

-------------------------
-- Constants
-------------------------

zeroTime :: UTCTime
zeroTime =
  UTCTime (ModifiedJulianDay 0)
          (secondsToDiffTime 0)

frameTime :: Double
frameTime = 1 / 60 -- seconds

playerTraceInterval :: NominalDiffTime
playerTraceInterval = 1 / 2 -- seconds

playerSize :: Box
playerSize = ((-0.5, -0.5), (0.5, 0.5))

eggSize :: Box
eggSize = ((-0.25, -0.25), (0.25, 0.25))

eggInitialVVel :: Double
eggInitialVVel =
  let v = 20 -- m/s
  in v * frameTime

eggGravity :: Double
eggGravity =
  let dv = 0.25 -- m/s^2
  in dv * frameTime

explosionSize :: Double
explosionSize = 2

explosionRate :: Double
explosionRate =
  let duration = 1.2 -- seconds
  in frameTime / duration

damageRate :: Double
damageRate =
  let dh = 0.2 -- /second
  in dh * frameTime

-------------------------
-- Set up
-------------------------

newGameChan :: IO GameChan
newGameChan = newChan

newGame :: [Box] -> GameState
newGame ws =
  GameState {gameRunning = True
            ,walls = ws
            ,objects = Map.empty
            ,traceQueue = []}

-------------------------
-- Modify
-------------------------

getObject :: ObjectId -> State GameState (Maybe GameObject)
getObject objectId =
  do game <- get
     return $ Map.lookup objectId (objects game)

addObject :: GameObject -> State GameState ObjectId
addObject object =
  do game <- get
     let objs = objects game
         lastId =
           if Map.null objs
              then 0
              else fst $ Map.findMax objs
         nextId = lastId + 1
         objects' =
           Map.insert nextId object objs
         game' = game {objects = objects'}
     put game'
     return nextId

removeObject :: ObjectId -> State GameState ()
removeObject objectId =
  do game <- get
     let objs = objects game
         objects' = Map.delete objectId objs
         game' = game {objects = objects'}
     put game'

updateObject :: (GameObject -> State GameState (Maybe GameObject))
             -> ObjectId
             -> State GameState ()
updateObject update objectId =
  do game <- get
     let objs = objects game
     case Map.lookup objectId objs of
       Nothing -> return ()
       Just object ->
         do object' <- update object
            let objects' =
                  case object' of
                    Just object' ->
                      Map.insert objectId object' objs
                    Nothing ->
                      Map.delete objectId objs
                game' = game {objects = objects'}
            put game'

updateObjects :: (GameObject -> State GameState (Maybe GameObject))
              -> State GameState ()
updateObjects update =
  do game <- get
     let keys = Map.keys $ objects game
     forM_ keys (updateObject update)

gameWalls :: State GameState [Box]
gameWalls = walls <$> get

getPlayer :: ObjectId -> State GameState (Maybe Player)
getPlayer playerId =
  do objs <- objects <$> get
     return $ Map.lookup playerId objs >>= getPlayer
  where getPlayer (PlayerObj player) = Just player
        getPlayer _ = Nothing

gamePlayers :: State GameState [(ObjectId,Player)]
gamePlayers =
  do objs <- objects <$> get
     return $ catMaybes $ map getPlayer (Map.assocs objs)
  where getPlayer (objectId,PlayerObj player) = Just (objectId, player)
        getPlayer _ = Nothing

gameEggs :: State GameState [(ObjectId,Egg)]
gameEggs =
  do objs <- objects <$> get
     return $ catMaybes $ map getEgg (Map.assocs objs)
  where getEgg (objectId,EggObj egg) = Just (objectId,egg)
        getEgg _ = Nothing

gameExplosions :: State GameState [(ObjectId,Explosion)]
gameExplosions =
  do objs <- objects <$> get
     return $ catMaybes $ map getExplosion (Map.assocs objs)
  where getExplosion (objectId,ExplosionObj explosion) = Just (objectId,explosion)
        getExplosion _ = Nothing

getPlayerId :: String -> State GameState (Maybe ObjectId)
getPlayerId name =
  do players <- gamePlayers
     let result = listToMaybe $ filter ((name ==) . playerName . snd) players
     return $ fst <$> result

addToTraceQueue :: ObjectId -> UTCTime -> State GameState ()
addToTraceQueue objectId time =
  modify (\game -> game {traceQueue = (time,objectId) : (traceQueue game)})

-------------------------
-- Input
-------------------------

processInput :: GameMsg -> State GameState ()
processInput Frame = return ()
processInput Stop =
  modify (\game -> game {gameRunning = False})

processInput (AddPlayer name chan) =
  let player =
        Player {playerName = name
               ,playerChan = chan
               ,playerPos = (0,0)
               ,playerVel = (0,0)
               ,playerBounds = playerSize
               ,playerHealth = 1}
  in do playerId <- addObject (PlayerObj player)
        addToTraceQueue playerId zeroTime

processInput (MovePlayer name (dx,dy)) =
  do Just playerId <- getPlayerId name
     updateObject updatePlayer playerId
  where updatePlayer (PlayerObj p) =
          let vel = (dx * frameTime,dy * frameTime)
          in return $ Just $ PlayerObj p {playerVel = vel}

processInput (RemovePlayer name) =
  do Just playerId <- getPlayerId name
     removeObject playerId

processInput (ThrowEgg owner (dx,dy)) =
  do Just playerId <- getPlayerId owner
     Just (PlayerObj player) <- getObject playerId
     let vel = (dx * frameTime,dy * frameTime)
         egg = Egg {eggPlayer = owner
                   ,eggPos = playerPos player
                   ,eggVel = vel
                   ,eggHeight = 0
                   ,eggVVel = eggInitialVVel
                   ,eggBounds = eggSize}
     addObject $ EggObj egg
     return ()

runInput :: GameChan -> GameState -> IO GameState
runInput chan game =
  do msg <- readChan chan
     case msg of
       Frame -> return game
       _ -> runInput chan $ execState (processInput msg) game

-------------------------
-- Simulation
-------------------------

startExplosion :: Pos -> State GameState ()
startExplosion pos =
  do let explosion =
           Explosion {explosionPos = pos
                     ,explosionT = 0
                     ,explosionBounds =
                        ((0,0),(0,0))}
     addObject $ ExplosionObj explosion
     return ()

damagePlayer :: ObjectId -> Double -> State GameState ()
damagePlayer playerId damage =
  updateObject update playerId
  where update (PlayerObj player) =
          let health = playerHealth player
              health' = health - damage
          in return $ Just (PlayerObj player {playerHealth = health'})

simulatePlayer :: Player -> State GameState (Maybe GameObject)
simulatePlayer player =
  do players <- map snd <$> gamePlayers
     walls <- gameWalls
     let pos = playerPos player
         vel = playerVel player
         bounds = playerBounds player
         otherPlayers = filter (/= player) players
         playersBoxes =
           map (boxAt <$> playerBounds <*> playerPos) otherPlayers
         boxes = playersBoxes ++ walls
         Trace pos' _ =
           trace pos bounds vel boxes
     return $ Just (PlayerObj player {playerPos = pos'})

simulateEgg :: Egg -> State GameState (Maybe GameObject)
simulateEgg egg =
  do players <- map snd <$> gamePlayers
     walls <- gameWalls
     let pos = eggPos egg
         vel = eggVel egg
         bounds = eggBounds egg
         playerBoxes =
           map (boxAt <$> playerBounds <*> playerPos) players
         boxes = playerBoxes ++ walls
         Trace pos' frac = trace pos bounds vel boxes
         vVel' = eggVVel egg - eggGravity
         height' = eggHeight egg + vVel'
     if frac < 1 || height' <= 0
        then do startExplosion pos'
                return Nothing
        else return $ Just (EggObj egg {eggPos = pos'
                                       ,eggVVel = vVel'
                                       ,eggHeight = height'})

simulateExplosion :: Explosion -> State GameState (Maybe GameObject)
simulateExplosion explosion =
  do let t = explosionT explosion
         s = (2 * t - 2 * t * t) * explosionSize
         bounds = ((-s,-s),(s,s))
         t' = t + explosionRate
     if t >= 1
        then return Nothing
        else return $
             Just (ExplosionObj explosion {explosionT = t'
                                          ,explosionBounds = bounds})

applyDamage :: State GameState ()
applyDamage =
  do players <- gamePlayers
     explosions <- map snd <$> gameExplosions
     let explosionBoxes = map (boxAt <$> explosionBounds <*> explosionPos) explosions
     forM_ players (doDamage explosionBoxes)
  where doDamage es (playerId,p) =
          let box = (boxAt <$> playerBounds <*> playerPos) p
              hits = length $ filter (boxesIntersect box) es
              damage= fromIntegral hits * damageRate
          in damagePlayer playerId damage

simulateObject :: GameObject -> State GameState (Maybe GameObject)
simulateObject (PlayerObj player) = simulatePlayer player
simulateObject (EggObj egg) = simulateEgg egg
simulateObject (ExplosionObj explosion) = simulateExplosion explosion

runSimulation :: State GameState ()
runSimulation =
  do updateObjects simulateObject
     applyDamage


-------------------------

toScene :: GameState -> Scene
toScene game =
  evalState buildScene game
  where buildScene =
          do players <- map snd <$> gamePlayers
             eggs <- map snd <$> gameEggs
             explosions <- map snd <$> gameExplosions
             return $ Scene {_walls = walls game
                            ,_players =
                              map ((,,) <$> playerName <*> playerPos <*> playerBounds) players
                            ,_eggs =
                              map ((,,) <$> eggPos <*> eggBounds <*> eggHeight) eggs
                            ,_explosions =
                              map ((,) <$> explosionPos <*> explosionBounds) explosions}

-------------------------
-- Trace
-------------------------

calculatePlayersSeen :: GameState -> Player -> [(String, Pos)]
calculatePlayersSeen game player =
  let players = snd <$> evalState gamePlayers game
      otherPlayers = filter (/= player) players
      candidates = map ((,) <$> playerName <*> playerPos) otherPlayers
      pos = playerPos player
      ws = walls game
  in filter (\(_,pos') -> canSee pos' pos ws) candidates

calculateEggsSeen :: GameState -> Player -> [Pos]
calculateEggsSeen game player =
  let eggs = snd <$> evalState gameEggs game
      candidates = eggPos <$> eggs
      pos = playerPos player
      ws = walls game
  in filter (\pos' -> canSee pos' pos ws) candidates

sendTrace :: GameState -> Player -> IO ()
sendTrace game player =
  do send (CurrentPos . playerPos)
     send (Health . playerHealth)
     send (PlayersSeen . calculatePlayersSeen game)
     send (EggsSeen . calculateEggsSeen game)
  where send f = writeChan (playerChan player) (f player)

sendRemoved :: Player -> IO ()
sendRemoved player =
  writeChan (playerChan player) Removed

runTraceQueue :: UTCTime -> State GameState [Player]
runTraceQueue t =
  do game <- get
     let (update, keep) = partition needsTrace (traceQueue game)
         updateIds = snd <$> update
     players <- mapM getPlayer updateIds
     let validIds = fst <$> filter (isJust . snd) (zip updateIds players)
         newEntries = map ((,) newTime) validIds
     put game {traceQueue = keep ++ newEntries}
     return $ catMaybes players
  where needsTrace (time,_) = time <= t
        newTime = addUTCTime playerTraceInterval t

runTrace :: GameState -> UTCTime -> IO GameState
runTrace game t =
  if not $ gameRunning game
     then do let players = snd <$> evalState gamePlayers game
             forM_ players sendRemoved
             return game
     else do let (players,game') = runState (runTraceQueue t) game
             forM_ players (sendTrace game')
             return game'

-------------------------

delayUntil :: UTCTime -> IO ()
delayUntil end =
  do start <- getCurrentTime
     let delay = diffUTCTime end start
     threadDelay . round $ delay * 1000000

runGame :: GameChan -> RenderChan -> GameState -> IO ()
runGame chan renderChan game =
  do t0 <- getCurrentTime
     runGame' chan renderChan game t0

runGame' :: GameChan -> RenderChan -> GameState -> UTCTime -> IO ()
runGame' chan renderChan game t0 =
  do writeChan chan Frame
     afterInput <- runInput chan game
     let game' = execState runSimulation afterInput
     _ <- sendScene renderChan (toScene game')
     game'' <- runTrace game' t0
     let t1 =
           addUTCTime (realToFrac frameTime)
                      t0
     delayUntil t1
     when (gameRunning game'') $ runGame' chan renderChan game'' t1
