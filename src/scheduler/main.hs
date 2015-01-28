-- Author: Markus Kusano
--
-- Main entry.
-- Processes command line args and then explores the concurrent program.
--
-- Currently, this uses the DPOR scheduler
--

import Options.Applicative.Extra (execParser)
import Opts (optParser, Options(..))
import DPOR (dporBegin)

mainOpts :: Options -> IO ()
mainOpts opts = do 
  dporBegin opts
  return ()

main :: IO()
main = execParser optParser >>= mainOpts 
