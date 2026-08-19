--  Pure colour mapping: typeclass-based colour schemes for escape-time values.
module Colour
  ( Colour(..)
  , ColourScheme(..)
  , Greyscale(..)
  , Fire(..)
  , Ocean(..)
  , colourize
  , colourToRGB
  , newtonColour
  ) where

import Data.Word (Word8)

--  An RGB triple with strict (unpacked) fields for performance.
data Colour = Colour !Word8 !Word8 !Word8
  deriving (Show, Eq)

--  Any type that can map an escape iteration to a Colour.
--  toColour scheme iter maxIter
class ColourScheme a where
  toColour :: a -> Int -> Int -> Colour

--  Linear greyscale: escaped-early pixels are black, interior is white.
data Greyscale = Greyscale

instance ColourScheme Greyscale where
  toColour Greyscale iter maxIter
    | iter >= maxIter = Colour 255 255 255
    | otherwise =
        let v = clampByte $ round (fromIntegral iter / fromIntegral maxIter * 255.0 :: Double)
        in  Colour v v v

--  Fire gradient: black → red → orange → yellow → white.
data Fire = Fire

instance ColourScheme Fire where
  toColour Fire iter maxIter
    | iter >= maxIter = Colour 0 0 0
    | otherwise       = fireGradient t
    where
      t :: Double
      t = fromIntegral iter / fromIntegral maxIter

      fireGradient :: Double -> Colour
      fireGradient x
        | x < 0.33  = lerpColour (x / 0.33)        (Colour 0 0 0)       (Colour 220 0 0)
        | x < 0.66  = lerpColour ((x - 0.33) / 0.33) (Colour 220 0 0)   (Colour 255 200 0)
        | otherwise  = lerpColour ((x - 0.66) / 0.34) (Colour 255 200 0) (Colour 255 255 255)

--  Ocean gradient: black → dark blue → cyan → white.
data Ocean = Ocean

instance ColourScheme Ocean where
  toColour Ocean iter maxIter
    | iter >= maxIter = Colour 0 0 0
    | otherwise       = oceanGradient t
    where
      t :: Double
      t = fromIntegral iter / fromIntegral maxIter

      oceanGradient :: Double -> Colour
      oceanGradient x
        | x < 0.33  = lerpColour (x / 0.33)          (Colour 0 0 0)       (Colour 0 0 180)
        | x < 0.66  = lerpColour ((x - 0.33) / 0.33)  (Colour 0 0 180)    (Colour 0 220 220)
        | otherwise  = lerpColour ((x - 0.66) / 0.34)  (Colour 0 220 220)  (Colour 255 255 255)

-- Linear interpolation between two colours by a fraction in [0, 1].
lerpColour :: Double -> Colour -> Colour -> Colour
lerpColour frac (Colour r0 g0 b0) (Colour r1 g1 b1) =
  Colour (interp r0 r1) (interp g0 g1) (interp b0 b1)
  where
    interp :: Word8 -> Word8 -> Word8
    interp a b = clampByte $ round (fromIntegral a + frac * (fromIntegral b - fromIntegral a :: Double))

clampByte :: Int -> Word8
clampByte n
  | n < 0     = 0
  | n > 255   = 255
  | otherwise = fromIntegral n

-- Convenience alias so call sites don't need to use the typeclass method name.
colourize :: ColourScheme a => a -> Int -> Int -> Colour
colourize = toColour

--  Colour a Newton fractal pixel by which root the orbit converged to (hue)
--  and how many iterations it took (brightness: fewer = brighter).
--
--  Root 0 → red, Root 1 → green, Root 2 → blue; non-convergent → black.
newtonColour :: (Int, Int) -> Int -> Colour
newtonColour (rootIdx, iters) maxI
  | iters >= maxI = Colour 0 0 0
  | otherwise     = case rootIdx of
      0 -> Colour shade 0     0
      1 -> Colour 0     shade 0
      2 -> Colour 0     0     shade
      _ -> Colour shade shade shade
  where
    t     = fromIntegral iters / fromIntegral maxI :: Double
    shade = clampByte $ round ((1.0 - t) * 255.0 :: Double)

--  Normalise a Colour to three Doubles in [0, 1] for Cairo's setSourceRGB.
colourToRGB :: Colour -> (Double, Double, Double)
colourToRGB (Colour r g b) =
  ( fromIntegral r / 255.0
  , fromIntegral g / 255.0
  , fromIntegral b / 255.0
  )
