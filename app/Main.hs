{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Data.Default
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as Bytes
import qualified Data.Text.IO as Text
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Number (decimal)

import LSC.BLIF    (parseBLIF)
import LEF.Parser  (parseLEF)
import LSC.Verilog (parseVerilog)

import LSC
import LSC.BLIF
import LSC.D3
import LSC.LEF
import LSC.SVG
import LSC.Types


type App = MaybeT IO
main :: IO ()
main = void $ runMaybeT program

exit :: App ()
exit = guard False

program :: App ()
program = do

  (flags, _) <- liftIO $ compilerFlags =<< getArgs
  opts <- liftIO $ compilerOpts flags

  let arg x = or [ k == x | (k, _) <- flags ]
      str x = head [ v | (k, v) <- flags, k == x ]

  when (null flags) exit

  -- print version string
  when (arg Version)
    $ do
      liftIO $ hPutStrLn stderr $ versionString
      exit

  -- generate registers
  when (and $ arg <$> [Register, Lef])
    $ do
      lef_ <- liftIO $ Text.readFile $ str Lef
      liftIO $ hPutStrLn stderr $ show $ parseLEF lef_
      exit

  -- json report
  when (and $ arg <$> [Json, Verilog])
    $ do
      verilog_ <- liftIO $ Text.readFile $ str Verilog
      liftIO $ Bytes.putStrLn $ encodeVerilog $ parseVerilog verilog_
      exit

   -- svg output
  when (and $ arg <$> [Lef, Blif, Compile])
    $ do
      net_ <- liftIO $ Text.readFile $ str Blif
      lef_ <- liftIO $ Text.readFile $ str Lef

      tech <- lift $ either
        (ioError . userError . show)
        (pure . fromLEF)
        (parseLEF lef_)
      netlist <- liftIO $ either
        (ioError . userError . show)
        (pure . gnostic tech . fromBLIF)
        (parseBLIF net_)

      circuit2d <- lift $ evalLSC opts tech $ compiler stage1 netlist

      liftIO $ plotStdout circuit2d

      exit

  when (arg Exline && not (arg Blif))
    $ do
      liftIO $ hPutStrLn stderr "exline: no blif given"
      exit

  when (arg Exline && not (arg Lef))
    $ do
      liftIO $ hPutStrLn stderr "exline: no lef given"
      exit

  when (and $ arg <$> [Json, Blif])
    $ do
      blif_ <- liftIO $ Text.readFile $ str Blif
      liftIO $ either
        (ioError . userError . show)
        (Bytes.putStrLn . encodeBLIF)
        (parseBLIF blif_)
      exit



type Flag = (FlagKey, FlagValue)

data FlagKey
  = Verbose
  | Version
  | Blif
  | Lef
  | Exline
  | Compile
  | Smt
  | Cores
  | Debug
  | Register
  | Rtl
  | Verilog
  | Json
  deriving (Eq, Show)

type FlagValue = String

args :: [OptDescr Flag]
args =
    [ Option ['v']      ["verbose"]    (NoArg  (Verbose, mempty))   "chatty output on stderr"
    , Option ['V', '?'] ["version"]    (NoArg  (Version, mempty))   "show version number"
    , Option ['b']      ["blif"]       (ReqArg (Blif, ) "FILE")     "BLIF file"
    , Option ['l']      ["lef"]        (ReqArg (Lef,  ) "FILE")     "LEF file"
    , Option ['d']      ["debug"]      (NoArg  (Debug, mempty))     "print some debug info"
    , Option ['c']      ["compile"]
        (OptArg ((Compile,  ) . maybe "svg" id) "svg,magic")        "output format"

    , Option ['x']      ["exline"]
        (OptArg ((Exline, ) . maybe "top" id)   "component")        "just exline and exit"

    , Option ['s']      ["smt"]        (ReqArg (Smt, ) "yices,z3")  "specify smt backend"
    , Option ['j']      ["cores"]      (ReqArg (Cores,  ) "count")  "limit number of cores"
    , Option ['J']      ["json"]       (NoArg  (Json, mempty))      "export json"
    , Option ['u']      ["verilog"]    (ReqArg (Verilog, ) "FILE")  "verilog file"
    , Option ['r']      ["register"]   (ReqArg (Register, ) "size in bits")  "generate register"
    ]


compilerOpts :: [Flag] -> IO CompilerOpts
compilerOpts xs = do
  n <- getNumCapabilities
  let j = last $ n : rights [ parse decimal "-j" v | (k, v) <- xs, k == Cores ]
  setNumCapabilities j
  ws <- createWorkers j
  let smt = last $ smtOption mempty : [ smtOption v | (k, v) <- xs, k == Smt ]
  pure $ def
    & enableDebug .~ elem Debug (fst <$> xs)
    & smtConfig .~ smt
    & workers .~ ws


compilerFlags :: [String] -> IO ([Flag], [String])
compilerFlags argv =
    case getOpt Permute args argv of
        (o, n, []  ) -> pure (o, n)
        (_, _, errs) -> mempty <$ hPutStrLn stderr (concat errs ++ usageInfo header args)
     where header = "Usage: lsc [arg...]"
