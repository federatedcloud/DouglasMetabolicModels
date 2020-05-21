module COBRA.Syntax where

import Data.Foldable (for_, traverse_)

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
