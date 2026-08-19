--  Pure image assembly and PPM I/O.
module Image
  ( applyColourScheme
  , toPPM
  , savePPM
  ) where

import Colour (Colour(..), ColourScheme, colourize)

--   applyColourScheme scheme maxIter escapeTimes maps every escape-time
--   value in the grid to a Colour via the supplied colour scheme.
--   Pure: no IO anywhere in this function.
applyColourScheme :: ColourScheme a => a -> Int -> [[Int]] -> [[Colour]]
applyColourScheme scheme maxI = map (map toC)
  where
    toC :: Int -> Colour
    toC iter = colourize scheme iter maxI

--   toPPM width height rows serialises a colour grid to a PPM P3 string.
--   Pure: returns a String; the caller decides what to do with it.
--
--   PPM P3 format:
--   
--   P3
--   <width> <height>
--   255
--   r g b  r g b  ...   (one row per line)
--   
toPPM :: Int -> Int -> [[Colour]] -> String
toPPM w h rows =
  unlines
    $  ["P3", show w ++ " " ++ show h, "255"]
    ++ map rowToLine rows
  where
    rowToLine :: [Colour] -> String
    rowToLine = unwords . map pixelStr

    pixelStr :: Colour -> String
    pixelStr (Colour r g b) = show r ++ " " ++ show g ++ " " ++ show b

--   savePPM path width height rows writes the PPM representation to path.
--   This is the only IO action in this module.
savePPM :: FilePath -> Int -> Int -> [[Colour]] -> IO ()
savePPM path w h rows = writeFile path $ toPPM w h rows
