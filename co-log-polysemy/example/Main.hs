module Main (main) where

import Prelude hiding (log)

import Polysemy (Member, Sem, runM)

import Colog (Message, richMessageAction, Severity (Debug, Info), Msg(..))
import Colog.Polysemy (Log, log, runLogAction)
import Data.Text ( Text )
import GHC.Stack (HasCallStack, withFrozenCallStack, callStack)

log' :: HasCallStack => Member (Log Message) r => Severity -> Text -> Sem r ()
log' msgSeverity msgText =
    withFrozenCallStack (log Msg { msgStack = callStack, .. })

example :: Member (Log Message) r => Sem r ()
example = do
    log' Debug "First message..."
    log' Info  "Second message..."

main :: IO ()
main = runM $ runLogAction @IO richMessageAction example
