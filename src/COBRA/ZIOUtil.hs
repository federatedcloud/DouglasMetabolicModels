{-# LANGUAGE NoImplicitPrelude #-}

module COBRA.ZIOUtil where

import           Path
import           ZIO.Trans


import Prelude as P

writeFile :: Path b File -> String -> ZIO r SomeNonPseudoException ()
writeFile fPath str  = zlift $ P.writeFile (toFilePath fPath) str

putStrLn :: String -> ZIO r SomeNonPseudoException ()
putStrLn x = zlift $ P.putStrLn x