module Main where

import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import System.Exit
import Control.Monad (when)
import DroidSignatureFileFilter

-- | Construct a usage message header.
header :: String -> String
header progName = "Usage: " ++ progName ++ " [options] [arguments]"

-- | Data type for command line options.
data Options = Options
    { optHelp  :: Bool
    , optPuids :: [String]
    } deriving (Show)

-- | Default vaules command line options.
defaultOptions :: Options
defaultOptions = Options
    { optHelp  = False
    , optPuids = []
    }

-- | Description of the command line options and how to merge the supplied
-- options with the default values by transforming an Options record.
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "show help message"
    , Option ['p'] ["puid"]
        (ReqArg (\p opts -> opts { optPuids = p:(optPuids opts) }) "PUID")
        "PUID to include in the output"
    -- TODO Add -P option to read PUIDs from file.
    ]

-- | Parse command line options.
parseArgs :: String -> [String] -> IO (Options, [String])
parseArgs hdr argv =
    case getOpt Permute options argv of
        (opts, args, []) -> return (foldr id defaultOptions opts, args)
        (_, _, errs)     -> error $ concat errs ++ usageInfo hdr options

main = do
    prg <- getProgName
    (opts, args) <- getArgs >>= parseArgs (header prg)
    when (optHelp opts) $ do
        putStr $ usageInfo (header prg) options
        exitSuccess
    -- TODO Default to STDIN if no file supplied.
    when (not $ null args) $ do
        content <- readFile $ head args
        putStrLn $ filterSigFile (optPuids opts) content

