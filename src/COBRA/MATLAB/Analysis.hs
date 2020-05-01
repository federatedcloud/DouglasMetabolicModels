module COBRA.MATLAB.Analysis where

import COBRA.Analysis
import COBRA.MATLAB.Util
import Control.Lens
import Foreign.Matlab
import Foreign.Matlab.Array

permListMX :: Int -> MIO [MXArray MDouble]
permListMX n = permList n ^.. folded . to (fmap fromIntegral) <&> travToMXArray & sequence
