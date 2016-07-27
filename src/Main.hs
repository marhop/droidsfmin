module Main where

import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
import System.Exit
import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import DroidSignatureFileFilter

-- | Construct a usage message header.
header :: String -> String
header progName = intercalate "\n"
    [ "The DROID Signature File Minimizer - filter a signature file based on"
    , "a list of PUIDs and keep only entries for those file formats that you"
    , "really need."
    , ""
    , "Usage: " ++ progName ++ " [options] [signature-file]"
    , ""
    ]

-- | Data type for command line options.
data Options = Options
    { optHelp       :: Bool
    , optPuids      :: [String]
    , optPuidsFile  :: Maybe FilePath
    , optList       :: Bool
    , optSupertypes :: Bool
    , optSubtypes   :: Bool
    , optOutFile    :: Maybe FilePath
    } deriving (Show)

-- | Default vaules for command line options.
defaultOptions :: Options
defaultOptions = Options
    { optHelp       = False
    , optPuids      = []
    , optPuidsFile  = Nothing
    , optList       = False
    , optSupertypes = False
    , optSubtypes   = False
    , optOutFile    = Nothing
    }

-- | Description of the command line options and how to merge the supplied
-- options with the default values by transforming an Options record.
options :: [OptDescr (Options -> Options)]
options =
    [ Option "h" ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "show help message"
    , Option "p" ["puid"]
        (ReqArg (\p opts -> opts { optPuids = p : optPuids opts }) "PUID")
        "include file format with this PUID in the output"
    , Option "P" ["puids-from-file"]
        (ReqArg (\f opts -> opts { optPuidsFile = Just f }) "FILE")
        "like -p, but read list of PUIDs from file (one PUID per line)"
    , Option "l" ["list"]
        (NoArg (\opts -> opts { optList = True }))
        "return a list of PUIDs instead of XML"
    , Option "S" ["include-supertypes"]
        (NoArg (\opts -> opts { optSupertypes = True }))
        "include file formats that are supertypes of the selected formats"
    , Option "s" ["include-subtypes"]
        (NoArg (\opts -> opts { optSubtypes = True }))
        "include file formats that are subtypes of the selected formats"
    , Option "o" ["output"]
        (ReqArg (\f opts -> opts { optOutFile = Just f }) "FILE")
        "output file"
    ]

-- | Parse command line options.
parseArgs :: String -> [String] -> (Options, Maybe FilePath)
parseArgs hdr argv =
    case getOpt Permute options argv of
        (os, as, []) -> (foldr id defaultOptions os, listToMaybe as)
        (_, _, es)   -> error $ concat es ++ usageInfo hdr options

-- | Read from a file or from STDIN if no file is specified.
input :: Maybe FilePath -> IO String
input (Just f) = readFile f
input Nothing  = getContents

-- | Read from a file or return an empty string if no file is specified.
input' :: Maybe FilePath -> IO String
input' (Just f) = readFile f
input' Nothing  = return ""

-- | Write to a file or to STDOUT if no file is specified.
output :: Maybe FilePath -> String -> IO ()
output (Just f) = writeFile f
output Nothing  = putStrLn

-- | Collect PUIDs from command line options -p and -P.
collectPuids :: Options -> IO [String]
collectPuids opts = do
    content <- input' $ optPuidsFile opts
    return $ optPuids opts ++ lines content

main = do
    prg <- getProgName
    (opts, sigFile) <- fmap (parseArgs $ header prg) getArgs
    when (optHelp opts) $ do
        putStr $ usageInfo (header prg) options
        exitSuccess
    content <- input sigFile
    puids <- collectPuids opts
    let filterOpts = [WithSupertypes | optSupertypes opts] ++
                     [WithSubtypes   | optSubtypes   opts]
    output (optOutFile opts) $
        (if optList opts then intercalate "\n" . listFileFormats else id) $
        filterSigFile filterOpts puids content

