{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import GHCJSDevServer.Client (runGHCJSDevServerClient)
import qualified HaskellFrontend

main :: IO ()
main = do
  runGHCJSDevServerClient 8081
  HaskellFrontend.main
