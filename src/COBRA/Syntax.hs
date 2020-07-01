module COBRA.Syntax where

import           Data.Foldable (for_, traverse_)
import           Foreign.Matlab.ZIOTypes
import           UnexceptionalIO (SomeNonPseudoException)
import           ZIO.Trans

-- | Note that if `f` is IO, the effect is not run,
-- | as it is not necessary in a lazy language.
-- (Currently unused)
-- absolveUnits :: f () -> ()
-- absolveUnits _ = ()

discard :: a -> ()
discard _ = ()

runAll :: Monad t => [t ()] -> t ()
{-# INLINE runAll #-}
runAll as = ufor_ as id

ufor_ :: (Foldable t, Applicative f) => t a -> (a -> f ()) -> f ()
{-# INLINE ufor_ #-}
ufor_ = for_

utraverse_ :: (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
{-# INLINE utraverse_ #-}
utraverse_ = traverse_

mayToEi :: e -> Maybe a -> Either e a
mayToEi err = maybe (Left err) Right

mapLast :: (a -> b) -> [a] -> Maybe b
mapLast f (x:[]) = Just $ f x
mapLast f (x:xs) = mapLast f xs
mapLast _ [] = Nothing

printLn :: String -> ZIO r MatlabException ()
printLn x = mxaeZ $ zlift $ putStrLn x