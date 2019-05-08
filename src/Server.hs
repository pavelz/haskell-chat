{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Server
    ( threadServer
    ) where

import Control.Exception (SomeException, toException)
import Control.Exception.Lifted (throwTo)
import Control.Monad.IO.Class (liftIO)
import Data.Text
import GHC.Stack (HasCallStack)
import System.IO (Handle, hFlush)
import TextUtils
import ThreadUtils
import Types
import Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStr)

{-
This thread polls the client's message queue and processes everything that comes down the queue.
It is named "threadServer" because this is where the bulk of server operations and logic reside.
But keep in mind that this function is executed for every client, and thus the code we write here is written from the standpoint of a single client (ie, the arguments to this function are the handle and message queue of a single client).
(Of course, we are in the "ChatStack" so we have access to the global shared state.)
-}
threadServer :: HasCallStack => Handle -> MsgQueue -> ChatStack () -- TODO: Handle exceptions.
threadServer h mq = readMsg mq >>= let loop = (>> threadServer h mq) in \case
  FromClient txt -> loop . interp mq $ txt
  FromServer txt -> loop . liftIO $ T.hPutStr h txt >> hFlush h
  Dropped        -> return () -- This kills the crab.

interp :: HasCallStack => MsgQueue -> Text -> ChatStack ()
interp mq txt = case T.toLower txt of
  "/quit"  -> send mq "See you next time!" >> writeMsg mq Dropped
  "/throw" -> throwToListenThread . toException $ PleaseDie -- For illustration/testing.
  _        -> send mq $ "I see you said, " <> dblQuote txt

{-
Note that we don't use the "Async" library here because the listen thread is the main thread: we didn't start it ourselves via the "async" function, and we have no "Async" data for it.
When you do have an "Async" object, you can use the "asyncThreadId" function to get the thread ID for the "Async".
-}
throwToListenThread :: HasCallStack => SomeException -> ChatStack ()
throwToListenThread e = maybeVoid (`throwTo` e) . listenThreadId =<< getState

