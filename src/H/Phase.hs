
module H.Phase
  ( Command
  , MainOptions(..)
  , PhasedCommandOptions(..)
  , phasedCommand
  , commandMain
  , module H.Monad
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.CmdTheLine

import H.Common
import H.Common.IO
import H.Monad
import qualified H.Phase.CmdLine as CL

type Command = (Term (IO ()), TermInfo)

data MainOptions =
  MainOptions
  { oName     :: String
  , oVersion  :: String
  , oCommands :: [Command]
  }

data PhasedCommandOptions n e m =
  PhasedCommandOptions
  { oPipeline :: FileMap T.Text -> MT n e m ()
  , oRunMonad :: forall a. m a -> IO a
  , oDefault  :: n
  , oCommand  :: String
  , oDoc      :: String
  }

phasedCommand
  :: (StageNames n, Show e, Monad m)
  => PhasedCommandOptions n e m
  -> Command
phasedCommand PhasedCommandOptions{..} = (term, info)
  where
    term = f oRunMonad oPipeline <$> CL.phase oDefault <*> CL.dump <*> CL.inputs
    info = defTI { termName = oCommand, termDoc = oDoc }
    f r p f ds inputs =
      readFiles (map decodeString inputs)
        >>= r . execMT (Options (Just f) (S.fromList ds)) . p
        >>= writeResults
        >>= exitWith

helpTerm :: MainOptions -> Term (IO ())
helpTerm mo = pure $ eval ["--help"] (pure (), info mo)

info :: MainOptions -> TermInfo
info MainOptions{..} = defTI { termName = oName, version = oVersion }

commandMain
  :: MainOptions
  -> IO ()
commandMain mo@MainOptions{..} =
  runChoice (helpTerm mo, info mo) oCommands

