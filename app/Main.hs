{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import GHC.Stack (HasCallStack)
import Listen
import Types

{-
To connect:
brew install telnet
telnet localhost 9696
-}

{-
Keep in mind that in Haskell, killing a parent thread does NOT kill its children threads!
Threads must be manually managed via the "Async" library. This takes careful thought and consideration.
Threads should never be "leaked:" we don't ever want a situation in which a child thread is left running and no other threads are aware of it.
Of course, the app should be architected in such a way that when an exception is thrown, it is handled gracefully. First and foremost, an exception must be caught on/by the thread on which the exception was thrown. If the exception represents something critical or unexpected (it's a bug, etc. - this is the vast majority of exceptions that we'll encounter in practice), and the exception occurs on a child thread, then the child thread should rethrow the exception to the listen (main) thread. The listen thread's exception handler should catch the rethrown exception and gracefully shut down, manually killing all child threads in the process.
-}

{-
TODO:
To fix the thread leakage bug:
* The "/throw" command should throw an exception on the server thread.
* The server thread's exception handler should catch the exception and throw an exception to the listen thread.
* The listen thread's exception handler should catch the exception and gracefully shut the server down by doing the following:
1) Put a "Msg" in every "MsgQueue" indicating that the server is shutting down.
2) Wait for every talk thread to finish.
-}

initChatState :: ChatState
initChatState = ChatState Nothing

main :: HasCallStack => IO ()
main = runReaderT threadListen =<< newIORef initChatState

