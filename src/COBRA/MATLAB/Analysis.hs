module COBRA.MATLAB.Analysis where

import           COBRA.Analysis
import           COBRA.MATLAB.Util
import           Control.Lens
import           Foreign.Matlab
import           Foreign.Matlab.ZIOArray
import           Foreign.Matlab.ZIOTypes
import           ZIO.Trans

permListMX :: Int -> ZIO r MatlabException [MXArray MDouble]
permListMX n = permList n ^.. folded . to (fmap fromIntegral) <&> travToMXArray & sequence
