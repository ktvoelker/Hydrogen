
module H.Prelude.Monads
  ( module Control.Monad.Except
  , module Control.Monad.Identity
  , module Control.Monad.Reader
  , module Control.Monad.State
  , modifyM, lift2, lift3, lift4
  ) where

import Control.Monad.Except hiding
  ( forM, forM_, mapM, mapM_, sequence, sequence_, msum )
import Control.Monad.Identity hiding
  ( forM, forM_, mapM, mapM_, sequence, sequence_, msum )
import Control.Monad.Reader hiding
  ( forM, forM_, mapM, mapM_, sequence, sequence_, msum )
import Control.Monad.State hiding
  ( forM, forM_, mapM, mapM_, sequence, sequence_, msum )

import H.Prelude.Core

-- | Modify the state of a StateT using a monadic action of the inner monad.
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM = (>>= put) . (get >>=) . (lift .)

lift2 :: (MonadTrans t, MonadTrans u, Monad m, Monad (u m)) => m a -> t (u m) a
lift2 = lift . lift

lift3
  :: ( MonadTrans t
     , MonadTrans u
     , MonadTrans v
     , Monad m
     , Monad (u m)
     , Monad (t (u m))
     )
  => m a
  -> v (t (u m)) a
lift3 = lift . lift . lift

lift4
  :: ( MonadTrans t
     , MonadTrans u
     , MonadTrans v
     , MonadTrans w
     , Monad m
     , Monad (u m)
     , Monad (t (u m))
     , Monad (v (t (u m)))
     )
  => m a
  -> w (v (t (u m))) a
lift4 = lift . lift . lift . lift

