--  Pure coordinate generation: maps pixel positions to complex-plane points.
module Grid
  ( Viewport(..)
  , pixelToComplex
  , generateGrid
  ) where

import Data.Complex (Complex(..))

--  Describes the rectangular region of the complex plane to render.
data Viewport = Viewport
  { vpWidth  :: Int
  , vpHeight :: Int
  , vpCenter :: (Double, Double)  -- ^ (real, imag) center of the view
  , vpZoom   :: Double
  } deriving (Show, Eq)

--  pixelToComplex vp x y maps pixel column x and row y to the
--   corresponding point on the complex plane.
--
--   The full image height spans 4/zoom complex units on the imaginary axis,
--   so at zoom 1 the classic [-2, 2] range is visible vertically.
pixelToComplex :: Viewport -> Int -> Int -> Complex Double
pixelToComplex vp x y = re :+ im
  where
    w        = vpWidth  vp
    h        = vpHeight vp
    (cx, cy) = vpCenter vp
    z        = vpZoom   vp
    unit     = 4.0 / (z * fromIntegral h)
    re       = cx + unit * (fromIntegral x - fromIntegral w / 2.0)
    im       = cy - unit * (fromIntegral y - fromIntegral h / 2.0)

--  generateGrid vp produces a row-major grid of complex-plane points
--   via a list comprehension — one inner list per image row.
generateGrid :: Viewport -> [[Complex Double]]
generateGrid vp =
  [ [ pixelToComplex vp x y | x <- [0 .. vpWidth vp - 1] ]
  | y <- [0 .. vpHeight vp - 1]
  ]
