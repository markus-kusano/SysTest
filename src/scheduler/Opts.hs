-- Author: Markus Kusano
--
-- Module containing command line parsing logic
--

module Opts(optParser, Options(..)) where
import Options.Applicative

optParser :: ParserInfo Options
optParser = info (helper <*> cmdParser)
    (fullDesc 
    <> progDesc "An explorer of a concurrent program's state space"
    <> header "SysTest: Systematic Concurrency Software Tester"
    )


data Options = Options { 
                       -- Verbose output (not yet implemented)
                       verbose :: Bool
                       -- Path to the program to execute. If it is empty,
                       -- then the program runs in standalone mode (the user
                       -- is required to execute the program)
                       , programPath :: String 
                       -- Arguments to pass to program under test
                       , programArgs :: [String]
                       } 

-- Parser for command line args
cmdParser :: Parser Options
cmdParser = 
  Options <$> switch 
                (short 'v'
              <> long "verbose"
              <> help "use verbose output")
           <*> argument str (metavar "PROG-NAME")
           <*> many (argument str (metavar "PROG-ARGUMENTS"))

