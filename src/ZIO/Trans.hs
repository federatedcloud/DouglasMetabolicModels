{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module ZIO.Trans (
    EIO, ZIO
  , elift, zlift
  , runZIO
  , withZIO
  , module Control.Monad.Reader

) where

import           Control.Monad.Except
import           Control.Monad.Fix
import           Control.Monad.IO.Class (MonadIO,)
import           Control.Monad.Reader hiding (lift)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           UnexceptionalIO hiding (fromIO, lift, run)
import           UnexceptionalIO.Trans (UIO, fromIO, run)

-- | Corresponds to IO[E, A] in Scala
newtype EIO e a = EIO { _unEIO :: ExceptT e UIO a }
  deriving ( Functor, Applicative, Monad, MonadFix, Unexceptional )

newtype ZIO r e a = ZIO { _unZIO :: ReaderT r (EIO e) a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadReader r, Unexceptional )

runZIO :: MonadIO m => ZIO r e a -> r -> (e -> m a) -> m a
runZIO app env handler = do
  let eio = runReaderT (_unZIO app) env
  resEi :: Either e a <- (run . runExceptT . _unEIO) eio
  either handler pure resEi


elift :: IO a -> EIO SomeNonPseudoException a
elift ioa =  EIO (fromIO ioa)

zlift :: IO a -> ZIO r SomeNonPseudoException a
zlift ioa = (ZIO . lift . elift) ioa

withZIO :: (SomeNonPseudoException -> e) -> ZIO r SomeNonPseudoException a -> ZIO r e a
withZIO etfun app = do
  r <- ask
  ZIO $ lift $ EIO $ ((withExceptT etfun) . _unEIO) (runReaderT (_unZIO app) r)
