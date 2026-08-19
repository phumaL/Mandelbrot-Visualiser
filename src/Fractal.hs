-- Pure fractal computation: escape-time algorithm for Mandelbrot and Julia sets.
module Fractal
  ( FractalType(..)
  , FractalConfig(..)
  , escapeTime
  , newtonTime
  , defaultConfig
  ) where

import Data.Complex (Complex(..), magnitude, realPart, imagPart)

-- Which fractal family to render.
data FractalType = Mandelbrot | Julia | Newton
  deriving (Show, Eq)

-- All parameters needed to render one fractal image.
data FractalConfig = FractalConfig
  { fractalType :: FractalType
  , center      :: (Double, Double)  -- ^ center of the view on the complex plane
  , zoom        :: Double
  , maxIter     :: Int
  , juliaC      :: (Double, Double)  -- ^ c constant for Julia sets (ignored for Mandelbrot)
  , startZ      :: (Double, Double)  -- ^ initial z₀ for Mandelbrot (normally (0,0))
  } deriving (Show, Eq)

-- Defaults for the initial GUI state.
defaultConfig :: FractalConfig
defaultConfig = FractalConfig
  { fractalType = Mandelbrot
  , center      = (0.0, 0.0)
  , zoom        = 1.0
  , maxIter     = 256
  , juliaC      = (-0.7, 0.27)
  , startZ      = (0.0, 0.0)
  }

-- | escapeTime cfg pt counts how many iterations the orbit of pt
--   takes to exceed the bailout radius 2, or returns maxIter cfg
--   if it never escapes.
--
--   * Mandelbrot: z₀ = startZ cfg,  c = pt
--   * Julia(k):   z₀ = pt,  c = k
--
--   Both share the iteration kernel z_{n+1} = z_n^2 + c.
--   Arithmetic is kept as raw Doubles to avoid Complex boxing overhead;
--   bailout uses |z|^4 > 4 rather than |z| > 2 to skip the sqrt.
escapeTime :: FractalConfig -> Complex Double -> Int
escapeTime cfg pt = go zr0 zi0 0
  where
    limit     = maxIter cfg
    pr        = realPart pt
    pi'       = imagPart pt
    (zr0, zi0, cr, ci) = case fractalType cfg of
      Mandelbrot -> let (zr, zi) = startZ cfg in (zr, zi, pr, pi')
      Julia      -> let (r, i) = juliaC cfg in (pr, pi', r, i)
      Newton     -> (0.0, 0.0, 0.0, 0.0)  -- Newton uses newtonTime, not escapeTime

    -- Tail-recursive loop over unboxed Doubles; no Complex allocation per step.
    go :: Double -> Double -> Int -> Int
    go zr zi n
      | n >= limit              = limit
      | zr*zr + zi*zi > 4.0    = n
      | otherwise               = go (zr*zr - zi*zi + cr) (2.0*zr*zi + ci) (n + 1)

--   newtonTime cfg pt applies Newton's method to f(z) = z³ − 1 starting at pt.
--   Returns (rootIndex, iterCount) where rootIndex ∈ {0,1,2} identifies which
--   cube root of unity the orbit converged to, or (0, maxIter cfg) on failure.
--
--   Iteration kernel: z_{n+1} = z − f(z)/f′(z) = z − (z³−1)/(3z²)
newtonTime :: FractalConfig -> Complex Double -> (Int, Int)
newtonTime cfg pt = go pt 0
  where
    limit = maxIter cfg
    tol   = 1e-6

    roots :: [Complex Double]
    roots = [ 1        :+ 0
            , (-0.5)   :+ ( sqrt 3 / 2)
            , (-0.5)   :+ (- (sqrt 3 / 2))
            ]

    go :: Complex Double -> Int -> (Int, Int)
    go z n
      | n >= limit              = (0, limit)
      | magnitude denom < 1e-10 = (0, limit)  
      | otherwise               = case nearestRoot z' of
          Just k  -> (k, n + 1)
          Nothing -> go z' (n + 1)
      where
        denom = 3 * z * z
        z'    = z - (z * z * z - 1) / denom

    nearestRoot :: Complex Double -> Maybe Int
    nearestRoot z =
      case filter (\(_, r) -> magnitude (z - r) < tol) (zip [0 ..] roots) of
        ((k, _) : _) -> Just k
        []           -> Nothing
