{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Types
    ( ChatStack
    , Env
    , MsgQueue
    , ChatState(..)
    , Msg(..)
    , PleaseDie(..)
    ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Exception (Exception)
import Control.Monad.Reader (ReaderT)
import Data.IntMap (IntMap)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable)

type ChatStack = ReaderT Env IO
type Env       = IORef ChatState
type MsgQueue  = TQueue Msg

data ChatState = ChatState { listenThreadId :: Maybe ThreadId
                           , talkAsyncs     :: IntMap (Async ()) }

data Msg = FromClient Text
         | FromServer Text
         | Dropped
         | Shutdown

data PleaseDie = PleaseDie deriving (Show, Typeable)
instance Exception PleaseDie
