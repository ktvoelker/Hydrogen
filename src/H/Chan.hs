
module H.Chan
  ( ReadChan()
  , WriteChan()
  , RChan(..)
  , WChan(..)
  , RWChan()
  , splitChan
  , newSplitChan
  , C.Chan
  , C.newChan
  ) where

import qualified Control.Concurrent.Chan as C

import H.Common
import H.Common.IO

newtype ReadChan a = ReadChan (IO a)

newtype WriteChan a = WriteChan (a -> IO ())

class RChan chan where
  readChan :: chan a -> IO a

class WChan chan where
  writeChan :: chan a -> a -> IO ()

class (RChan chan, WChan chan) => RWChan chan where

splitChan :: C.Chan a -> (ReadChan a, WriteChan a)
splitChan chan = (ReadChan (C.readChan chan), WriteChan (C.writeChan chan))

newSplitChan :: IO (ReadChan a, WriteChan a)
newSplitChan = splitChan <$> C.newChan

instance RChan ReadChan where
  readChan (ReadChan r) = r

instance WChan WriteChan where
  writeChan (WriteChan w) = w

instance RChan C.Chan where
  readChan = C.readChan

instance WChan C.Chan where
  writeChan = C.writeChan

instance RWChan C.Chan where

