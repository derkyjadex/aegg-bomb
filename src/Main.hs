import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Function (fix)
import Physics
import Graphics

data Msg = Text Int String
         | Quit

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

runGame :: Chan GameMsg -> RenderChan -> GameState -> IO ()
runGame chan renderChan game = do
  writeChan chan Frame
  game' <- liftM runPhysics $ runInput chan game
  runRender game' renderChan
  runTrace game'
  threadDelay (500 * 1000)
  if gameRunning game
    then runGame chan renderChan game'
    else return ()

runServer :: RenderChan -> IO ()
runServer renderChan = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  gameChan <- newChan
  game <- forkIO $ runGame gameChan renderChan newGame
  acceptor <- forkIO $ runAccept sock gameChan
  runConsole
  writeChan gameChan Stop
  killThread game
  killThread acceptor
  close sock

runConsole :: IO ()
runConsole = do
  putStr "> "
  cmd <- getLine
  case cmd of
    "quit" -> return ()
    _ -> do
      putStrLn "Unknown command"
      runConsole

runAccept :: Socket -> Chan GameMsg -> IO ()
runAccept sock chan = do
  conn <- accept sock
  forkIO $ runConnection conn chan
  runAccept sock chan

runConnection :: (Socket, SockAddr) -> Chan GameMsg -> IO ()
runConnection (sock, addr) gameChan = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  hPutStrLn h "Hi, what's your name?"
  name <- hGetLine h
  hPutStrLn h $ "Welcome, " ++ name ++ "!"
  playerChan <- newChan
  writeChan gameChan $ AddPlayer name playerChan

  reader <- forkIO $ forever $ do
    msg <- readChan playerChan
    case msg of
      NothingSeen pos -> hPutStrLn h $ "Nothing seen from " ++ show pos
      Removed -> hClose h

  handle (\ex@(SomeException _) -> return ()) $ forever $ do
    cmd <- liftM maybeRead $ hGetLine h
    case cmd of
      Just (Move vel) -> writeChan gameChan $ MovePlayer name vel
      Nothing -> hPutStrLn h "Unknown command"

  killThread reader
  writeChan gameChan $ RemovePlayer name
  hClose h

maybeRead :: Read a => String -> Maybe a
maybeRead s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

main :: IO ()
main = do
  chan <- newRenderChan
  forkIO $ runServer chan
  renderMain chan
