{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module ThreadUtils
    ( getState
    , modifyState
    , readMsg
    , writeMsg
    , send
    , receive
    , runAsync
    ) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Text
import GHC.Stack (HasCallStack)
import Types
import TextUtils

{-
The Haskell language, software transactional memory, and this app are all architected in such a way that we need not concern ourselves with the usual pitfalls of sharing state across threads. This automatically rules out a whole class of potential bugs! Just remember the following:
1) If you are coding an operation that simply needs to read the state, use the "getState" helper function.
2) If you are coding an operation that needs to update the state, you must bundle the operation into an atomic unit: that is, a single function passed into the "modifyState" helper function. (The function passed to "modifyState" is itself passed the latest state.)
It's ensured that only one thread can modify the state (via "modifyState") at a time.
-}
getState :: HasCallStack => ChatStack ChatState
getState = liftIO . readIORef =<< ask

modifyState :: HasCallStack => (ChatState -> (ChatState, a)) -> ChatStack a
modifyState f = ask >>= \ref -> liftIO . atomicModifyIORef' ref $ f

readMsg :: HasCallStack => MsgQueue -> ChatStack Msg
readMsg = liftIO . atomically . readTQueue

writeMsg :: HasCallStack => MsgQueue -> Msg -> ChatStack ()
writeMsg mq = liftIO . atomically . writeTQueue mq

send :: HasCallStack => MsgQueue -> Text -> ChatStack ()
send mq = writeMsg mq . FromServer . nl

receive :: HasCallStack => MsgQueue -> Text -> ChatStack ()
receive mq = writeMsg mq . FromClient

-- Spawn a new thread in the "ChatStack".
runAsync :: HasCallStack => ChatStack () -> ChatStack (Async ())
runAsync f = liftIO . async . runReaderT f =<< ask
