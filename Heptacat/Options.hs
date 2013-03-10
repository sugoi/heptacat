{-# LANGUAGE DeriveDataTypeable #-}
module Heptacat.Options where

import           Data.Data
import qualified System.Console.CmdArgs.Implicit as Opt
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Explicit as Opt
  (processArgs, helpText, HelpFormat(..))
import           System.Exit
import           System.IO.Unsafe (unsafePerformIO)
import           Data.String.Utils (split)

data MyOptions
  = Worker { subjectRepository :: String, logRepository :: String }
  | Init
  | Help { helpItem :: String }
  deriving (Show, Data,Typeable)


{-# NOINLINE myOptions #-}
myOptions :: MyOptions
myOptions =
  unsafePerformIO $ do
    parser <- Opt.processArgs heptacat
    args <- Opt.cmdArgsApply parser

    case args of
      Help itemStr -> do
        let xs = filter ((==itemStr) . fst) itemHelp
        case xs of
          ((_,x):_) | not (null xs) -> putStrLn x
          _                     -> print $ Opt.helpText [] Opt.HelpFormatOne heptacat
        _ <- exitSuccess
        return undefined
      _ -> return args

  where
    heptacat :: Opt.Mode (Opt.CmdArgs MyOptions)
    heptacat = Opt.cmdArgsMode $ myModes
       &= Opt.program "* heptacat" &= Opt.summary "heptacat (C) Takayuki Muranushi 2013"

    progName :: String
    progName = "* heptacat"

    itemHelp :: [(String, String)]
    itemHelp =
      concat $
      map parseItemHelp $
      split progName $ show heptacat

    parseItemHelp str =
      case words str of
        [] -> []
        (cmd:_) -> [(cmd,progName ++ " " ++ str)]


    myModes = Opt.modes
      [Init  &= Opt.help "initialize log repository",
       Help { helpItem = Opt.def &= Opt.typ "item" &= Opt.argPos 0 &= Opt.opt ""}
         &= Opt.help "display help message (for an item)",
       Worker { subjectRepository = Opt.def &= Opt.argPos 0 &= Opt.typ "subjRep" ,
                logRepository = Opt.def &= Opt.argPos 1 &= Opt.typ "logRep"
              }
       &= Opt.help "start worker"
       &= Opt.details ["start a heptacat worker with given subject repository and log repository. The repository URLs must be of the form git@server.addr:subjRep/repository/url and git@server.addr:log/repository/url, respectively."]
       ]
