module Main (main) where

import GHC.Conc      (getNumProcessors, setNumCapabilities)
import GUI           (runGUI)

main :: IO ()
main = do
  n <- getNumProcessors
  setNumCapabilities n
  runGUI
