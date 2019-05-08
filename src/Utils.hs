
module Utils
    ( mIf
    , maybeVoid
    ) where

import GHC.Stack (HasCallStack)
import Data.Bool (bool)


-- Monadic "if".
mIf :: (HasCallStack, Monad m) => m Bool -> m a -> m a -> m a
mIf p x = (p >>=) . flip bool x

-- Operate on a "Maybe" in a monad. Do nothing on "Nothing".
maybeVoid :: (HasCallStack, Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeVoid = maybe (return ())

