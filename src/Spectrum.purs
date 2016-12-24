module Spectrum
  ( scene
  , module Graphics.Vis
  ) where

import Prelude

import Color (Color, hsl)
import Data.Array ((..), index, length, range)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Graphics.Vis (FFT, animate, graphics)
import Graphics.Drawing (Drawing, filled, fillColor, rectangle)

-- | Run this in PSCi by importing this module and
-- | evaluating `animate scene`.
scene :: Number -> FFT -> Drawing
scene t fft =
    let fftValue = index fft >>> fromMaybe 0.0
        barHeight idx = -1.5 * (fftValue idx) * (1.0 + 0.15 * (toNumber idx))
        barColor = barHeight >>> hue >>> fillColor
        barX = (*) 40 >>> toNumber >>> (+) 100.0
        barShape idx = rectangle (barX idx) 600.0 20.0 (barHeight idx)
        barDrawing idx = filled (barColor idx) (barShape idx) in
    fold $ map barDrawing $ range 1 $ length fft

hue :: Number -> Color
hue value = hsl value (0.5) (0.5)
