module WASHHelper (
  run,
  module WASH.CGI.CGI
) where

import System.Environment
import System.Posix.Env
import List
import WASH.CGI.CGI hiding (run)
import qualified WASH.CGI.CGI

-- Some hacks are required to make WASH working with the Python webserver.
run :: CGI () -> IO ()
run f = 
  do unsetEnv "CONTENT_TYPE"
     args <- getArgs
     withArgs (filter (not . null) args) $ WASH.CGI.CGI.run f
