
module H.Phase.CmdLine where

import qualified Data.Map as M
import System.Console.CmdTheLine
import Text.PrettyPrint

import H.Common

inputs :: Term [String]
inputs =
  value
  . posAny []
  $ posInfo { posName = "INPUT", posDoc = "Use these files as inputs" }

showStageName :: (StageNames n) => n -> String
showStageName = map toLower . show

phases :: (StageNames n) => M.Map String n
phases =
  M.fromList
  . map (\n -> (showStageName n, n))
  $ [minBound .. maxBound]

newtype PhaseArg n = PhaseArg { getStageName :: n }

deriving instance (Eq n) => Eq (PhaseArg n)

deriving instance (Ord n) => Ord (PhaseArg n)

instance (StageNames n) => ArgVal (PhaseArg n) where
  converter = (parser, printer)
    where
      parser xs =
        maybe (Left . badPhase $ xs) (Right . PhaseArg)
        . flip M.lookup phases
        . map toLower
        $ xs
      printer = text . showStageName . getStageName

badPhase :: String -> Doc
badPhase name = text $ "Unknown phase: " ++ name

dump :: (StageNames n) => Term [n]
dump =
  (map getStageName <$>)
  . value
  . optAll []
  $ (optInfo ["dump", "d"])
    { optName = "DUMP"
    , optDoc  = "Print the output of this phase"
    }

phase :: (StageNames n) => n -> Term n
phase def =
  (getStageName <$>)
  . value
  . opt (PhaseArg def)
  $ (optInfo ["phase", "p"])
    { optName = "PHASE"
    , optDoc  = "Stop after this phase"
    }

instance (ArgVal (PhaseArg n)) => ArgVal (Maybe (PhaseArg n)) where
  converter = (parser, printer)
    where
      parser xs = case fst converter xs of
        Left err  -> Left err
        Right val -> Right (Just val)
      printer Nothing = text "Nothing"
      printer (Just pa) = text "Just" <+> snd converter pa

