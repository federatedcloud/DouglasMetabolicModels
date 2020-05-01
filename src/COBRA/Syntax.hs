module COBRA.Syntax where

-- | Note that if `f` is IO, the effect is not run,
-- | as it is not necessary in a lazy language.
absolveUnits :: f () -> ()
absolveUnits _ = ()

discard :: a -> ()
discard _ = ()

runAll :: Monad t => [t ()] -> t ()
runAll as = absolveUnits <$> sequence as
