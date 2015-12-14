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
import           System.Random

type ObjectId = Int

data Game =
  Game {gameRunning   :: Bool
       ,walls         :: [Box]
       ,bounds        :: Box
       ,objects       :: Map ObjectId GameObject
       ,statusQueue   :: [(UTCTime,ObjectId)]
       ,randGen       :: StdGen
       ,killedPlayers :: [(Player,String)]
       ,scores        :: Map String Int}

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
  Explosion {explosionPlayer :: String
            ,explosionPos    :: Pos
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
  = Status Pos Double [(String,Pos)] [Pos]
  | Killed String
  | Removed
  deriving (Eq)

data PlayerCmd
  = Move Double Double
  | Throw Double Double
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

playerStatusInterval :: NominalDiffTime
playerStatusInterval = 1 / 2 -- seconds

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

newGame :: [Box] -> Game
newGame ws =
  let xs = (fst . fst <$> ws) ++ (fst . snd <$> ws)
      ys = (snd . fst <$> ws) ++ (snd . snd <$> ws)
  in Game {gameRunning = True
          ,walls = ws
          ,bounds = ((minimum xs, minimum ys), (maximum xs, maximum ys))
          ,objects = Map.empty
          ,statusQueue = []
          ,randGen = mkStdGen 405968
          ,killedPlayers = []
          ,scores = Map.empty}

-------------------------
-- Modify
-------------------------

getObject :: ObjectId -> State Game (Maybe GameObject)
getObject objectId =
  do objs <- objects <$> get
     return $ Map.lookup objectId objs

addObject :: GameObject -> State Game ObjectId
addObject object =
  do objs <- objects <$> get
     let lastId =
           if Map.null objs
              then 0
              else fst $ Map.findMax objs
         nextId = lastId + 1
         objects' =
           Map.insert nextId object objs
     modify (\game -> game {objects = objects'})
     return nextId

removeObject :: ObjectId -> State Game ()
removeObject objectId =
  do objs <- objects <$> get
     let objects' = Map.delete objectId objs
     modify (\game -> game {objects = objects'})

updateObject :: (GameObject -> State Game (Maybe GameObject))
             -> ObjectId
             -> State Game ()
updateObject update objectId =
  do objs <- objects <$> get
     case Map.lookup objectId objs of
       Nothing -> return ()
       Just object ->
         do object' <- update object
            objs <- objects <$> get
            let objects' =
                  case object' of
                    Just object' ->
                      Map.insert objectId object' objs
                    Nothing ->
                      Map.delete objectId objs
            modify (\game -> game {objects = objects'})

updateObjects :: (GameObject -> State Game (Maybe GameObject))
              -> State Game ()
updateObjects update =
  do objs <- objects <$> get
     let keys = Map.keys objs
     forM_ keys (updateObject update)

gameWalls :: State Game [Box]
gameWalls = walls <$> get

getPlayer :: ObjectId -> State Game (Maybe Player)
getPlayer playerId =
  do objs <- objects <$> get
     return $ Map.lookup playerId objs >>= getPlayer
  where getPlayer (PlayerObj player) = Just player
        getPlayer _ = Nothing

gamePlayers :: State Game [(ObjectId,Player)]
gamePlayers =
  do objs <- objects <$> get
     return $ catMaybes $ map getPlayer (Map.assocs objs)
  where getPlayer (objectId,PlayerObj player) = Just (objectId, player)
        getPlayer _ = Nothing

gameEggs :: State Game [(ObjectId,Egg)]
gameEggs =
  do objs <- objects <$> get
     return $ catMaybes $ map getEgg (Map.assocs objs)
  where getEgg (objectId,EggObj egg) = Just (objectId,egg)
        getEgg _ = Nothing

gameExplosions :: State Game [(ObjectId,Explosion)]
gameExplosions =
  do objs <- objects <$> get
     return $ catMaybes $ map getExplosion (Map.assocs objs)
  where getExplosion (objectId,ExplosionObj explosion) = Just (objectId,explosion)
        getExplosion _ = Nothing

getPlayerId :: String -> State Game (Maybe ObjectId)
getPlayerId name =
  do players <- gamePlayers
     let result = listToMaybe $ filter ((name ==) . playerName . snd) players
     return $ fst <$> result

addToStatusQueue :: ObjectId -> UTCTime -> State Game ()
addToStatusQueue objectId time =
  modify (\game -> game {statusQueue = (time,objectId) : (statusQueue game)})

findFreeSpace :: Box -> State Game Pos
findFreeSpace box =
  do ((minX, minY), (maxX, maxY)) <- bounds <$> get
     walls <- gameWalls
     rand <- randGen <$> get
     let (x, rand') = randomR (minX, maxX) rand
         (y, rand'') = randomR (minY, maxY) rand'
         testBox = boxAt box (x, y)
         isFree = not $ any (boxesIntersect testBox) walls
     modify (\game -> game {randGen = rand''})
     if isFree
       then return (x,y)
       else findFreeSpace box

givePoints :: String -> Int -> State Game ()
givePoints name points =
  do scores <- scores <$> get
     modify $ \game ->
       game {scores = Map.adjust (+ points) name scores}

-------------------------
-- Input
-------------------------

processInput :: GameMsg -> State Game ()
processInput Frame = return ()
processInput Stop =
  modify (\game -> game {gameRunning = False})

processInput (AddPlayer name chan) =
  do pos <- findFreeSpace playerSize
     let player =
           Player {playerName = name
                  ,playerChan = chan
                  ,playerPos = pos
                  ,playerVel = (0,0)
                  ,playerBounds = playerSize
                  ,playerHealth = 1}
     playerId <- addObject (PlayerObj player)
     addToStatusQueue playerId zeroTime
     modify $ \game ->
       game {scores = Map.insert name 0 (scores game)}

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

runInput :: GameChan -> Game -> IO Game
runInput chan game =
  do msg <- readChan chan
     case msg of
       Frame -> return game
       _ -> runInput chan $ execState (processInput msg) game

-------------------------
-- Simulation
-------------------------

startExplosion :: Pos -> String -> State Game ()
startExplosion pos player =
  do let explosion =
           Explosion {explosionPlayer = player
                     ,explosionPos = pos
                     ,explosionT = 0
                     ,explosionBounds =
                        ((0,0),(0,0))}
     addObject $ ExplosionObj explosion
     return ()

damagePlayer :: ObjectId -> Double -> String -> State Game ()
damagePlayer playerId damage attacker =
  do Just player <- getPlayer playerId
     let health = playerHealth player
         health' = health - damage
     if health' > 0
        then updatePlayer player {playerHealth = health'}
        else do pos <- findFreeSpace playerSize
                updatePlayer player {playerPos = pos
                                    ,playerHealth = 1}
                givePoints attacker 2
                givePoints (playerName player) (-1)
                modify $ \game ->
                  game {killedPlayers = (player,attacker) : (killedPlayers game)}

  where updatePlayer p =
          updateObject (\_ -> return $ Just (PlayerObj p)) playerId

simulatePlayer :: Player -> State Game (Maybe GameObject)
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

simulateEgg :: Egg -> State Game (Maybe GameObject)
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
        then do startExplosion pos' (eggPlayer egg)
                return Nothing
        else return $ Just (EggObj egg {eggPos = pos'
                                       ,eggVVel = vVel'
                                       ,eggHeight = height'})

simulateExplosion :: Explosion -> State Game (Maybe GameObject)
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

applyDamage :: State Game ()
applyDamage =
  do explosions <- map snd <$> gameExplosions
     forM_ explosions $ \explosion ->
       do players <- gamePlayers
          let box = (boxAt <$> explosionBounds <*> explosionPos) explosion
              attacker = explosionPlayer explosion
              hits = filter ((boxesIntersect box) . (boxAt <$> playerBounds <*> playerPos) . snd) players
          forM_ (fst <$> hits) $ \playerId ->
            damagePlayer playerId damageRate attacker

simulateObject :: GameObject -> State Game (Maybe GameObject)
simulateObject (PlayerObj player) = simulatePlayer player
simulateObject (EggObj egg) = simulateEgg egg
simulateObject (ExplosionObj explosion) = simulateExplosion explosion

runSimulation :: State Game ()
runSimulation =
  do updateObjects simulateObject
     applyDamage


-------------------------

toScene :: Game -> Scene
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
                              map ((,) <$> explosionPos <*> explosionBounds) explosions
                            ,_scores = Map.assocs (scores game)}

-------------------------
-- Status
-------------------------

calculatePlayersSeen :: Game -> Player -> [(String, Pos)]
calculatePlayersSeen game player =
  let players = snd <$> evalState gamePlayers game
      otherPlayers = filter (/= player) players
      candidates = map ((,) <$> playerName <*> playerPos) otherPlayers
      pos = playerPos player
      ws = walls game
  in filter (\(_,pos') -> canSee pos' pos ws) candidates

calculateEggsSeen :: Game -> Player -> [Pos]
calculateEggsSeen game player =
  let eggs = snd <$> evalState gameEggs game
      candidates = eggPos <$> eggs
      pos = playerPos player
      ws = walls game
  in filter (\pos' -> canSee pos' pos ws) candidates

sendStatus :: Game -> Player -> IO ()
sendStatus game player =
  let pos = playerPos player
      health = playerHealth player
      players = calculatePlayersSeen game player
      eggs = calculateEggsSeen game player
  in writeChan (playerChan player) (Status pos health players eggs)

sendRemoved :: Player -> IO ()
sendRemoved player =
  writeChan (playerChan player) Removed

runStatusQueue :: UTCTime -> State Game [Player]
runStatusQueue t =
  do queue <- statusQueue <$> get
     let (update, keep) = partition needsStatus queue
         updateIds = snd <$> update
     players <- mapM getPlayer updateIds
     let validIds = fst <$> filter (isJust . snd) (zip updateIds players)
         newEntries = map ((,) newTime) validIds
     modify (\game -> game {statusQueue = keep ++ newEntries})
     return $ catMaybes players
  where needsStatus (time,_) = time <= t
        newTime = addUTCTime playerStatusInterval t

runStatus :: Game -> UTCTime -> IO Game
runStatus game t =
  if not $ gameRunning game
     then do let players = snd <$> evalState gamePlayers game
             forM_ players sendRemoved
             return game
     else do let (players,game') = runState (runStatusQueue t) game
             forM_ players (sendStatus game')
             return game'

runKilledPlayers :: Game -> IO Game
runKilledPlayers game =
  do forM_ (killedPlayers game) $ \(player,attacker) ->
       writeChan (playerChan player) (Killed attacker)
     return game {killedPlayers = []}

-------------------------

delayUntil :: UTCTime -> IO ()
delayUntil end =
  do start <- getCurrentTime
     let delay = diffUTCTime end start
     threadDelay . round $ delay * 1000000

runGame :: GameChan -> RenderChan -> Game -> IO ()
runGame chan renderChan game =
  do t0 <- getCurrentTime
     runGame' chan renderChan game t0

runGame' :: GameChan -> RenderChan -> Game -> UTCTime -> IO ()
runGame' chan renderChan game t0 =
  do writeChan chan Frame
     afterInput <- runInput chan game
     let game' = execState runSimulation afterInput
     _ <- sendScene renderChan (toScene game')
     game'' <- runStatus game' t0
     game''' <- runKilledPlayers game''
     let t1 =
           addUTCTime (realToFrac frameTime)
                      t0
     delayUntil t1
     when (gameRunning game''') $ runGame' chan renderChan game''' t1
