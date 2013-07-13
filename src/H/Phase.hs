
module H.Phase
  ( InputFile(..)
  , MainOptions(..)
  , phasedMain
  , module H.Monad
  ) where

import qualified Data.Set as S
import System.Console.CmdTheLine (Term, run, defTI, termName, version)
import System.IO

import H.Common
import H.Common.IO
import H.Monad
import qualified H.Phase.CmdLine as CL

data InputFile =
  InputFile
  { ifName :: String
  , ifText :: String
  } deriving (Eq, Ord, Show)

argMain
  :: (Show e, StageNames n, Monad' m)
  => (forall a. m a -> IO a)        -- ^ The runner for the user monad
  -> ([InputFile] -> MT n e m ())   -- ^ The pipeline
  -> n                              -- ^ The name of the final phase to run
  -> [n]                            -- ^ The names of phases whose output to dump
  -> [String]                       -- ^ The names of the input files
  -> IO ()
argMain r p f ds inputs =
  mapM (\name -> InputFile name <$> readFile name) inputs
    >>= r . execMT (Options (Just f) (S.fromList ds)) . p
    >>= writeResults
    >>= exitWith

data MainOptions n e m =
  MainOptions
  { moPipeline :: [InputFile] -> MT n e m ()
  , moName     :: String
  , moVersion  :: String
  , moRunMonad :: forall a. m a -> IO a
  }

termMain
  :: (StageNames n, Show e, Monad' m)
  => (forall a. m a -> IO a)
  -> ([InputFile] -> MT n e m ())
  -> Term (IO ())
termMain r p = argMain r p <$> CL.phase <*> CL.dump <*> CL.inputs

phasedMain
  :: (StageNames n, Show e, Monad' m)
  => MainOptions n e m
  -> IO ()
phasedMain MainOptions{..} =
  run
    ( termMain moRunMonad moPipeline
    , defTI { termName = moName, version = moVersion }
    )

