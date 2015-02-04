import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import System.IO
import Text.ParserCombinators.Parsec

import WurmParser

data Environment = Environment { _eventLog :: Handle
                               }

data ProgramState = ProgramState { _queueLength :: Int
                                 }

-- No idea why this doesn't work :'(
--newtype WurmHelper a = WurmHelper { asdf :: StateT ProgramState (ReaderT Environment IO) a
--                                  } deriving (Monad, MonadIO, MonadReader ProgramState, MonadState ProgramState)

type WurmHelper a = StateT ProgramState (ReaderT Environment IO) a

runWurmHelper :: WurmHelper a -> Environment -> ProgramState -> IO a
runWurmHelper wh env initState = runReaderT (evalStateT wh initState) env

beep :: IO ()
beep = putStr "Beep!\n\a"

startMessage :: WurmHelper ()
startMessage = do
    liftIO $ putStrLn "Start"
    state <- get
    let queueLength = _queueLength state
    when (queueLength == 0) $ put (state{_queueLength = 1})

repeatMessage :: WurmHelper ()
repeatMessage = liftIO (putStrLn "Repeat") >> modify updateQueueLength
    where 
        increase :: (Num a, Eq a) => a -> a
        increase a = if a == 0 then 2 else a+1

        updateQueueLength :: ProgramState -> ProgramState
        updateQueueLength a = a{_queueLength = increase $ _queueLength a}

endMessage :: WurmHelper ()
endMessage = do
    liftIO $ putStrLn "End"
    state <- get
    let queueLength = _queueLength state
    liftIO $ when (queueLength == 1) beep
    put (state{_queueLength = _queueLength state - 1})

processMessage :: String -> WurmHelper ()
processMessage msg = either printError (\message ->
        case message of
            ActionRepeat -> repeatMessage
            ActionStart -> startMessage
            ActionEnd -> endMessage
            _ -> return ()
        ) $ parse parseEvent "Event" msg

    where printError s = liftIO $ print s
    

mainLoop :: WurmHelper ()
mainLoop = do
    eventHandle <- _eventLog <$> ask
    eventEOF <- liftIO $ hIsEOF eventHandle
    unless eventEOF (liftIO (hGetLine eventHandle) >>= processMessage)
    mainLoop

main :: IO ()
main = do
    handle <- openFile "/home/xandaros/wurm/players/Xyonado/logs/_Event.2015-02.txt" ReadMode
    hSeek handle SeekFromEnd 0
    runWurmHelper mainLoop (Environment handle) (ProgramState 0)
