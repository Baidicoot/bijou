{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Datatypes.Lam
import Datatypes.Prim
import ClosureConv
import PartialApp
import Parser
import ANFify
import Backend

import System.Environment

buildFile :: String -> String -> IO ()
buildFile root file = do
    str <- readFile (root ++ file)
    case parseLamDefs (root ++ file) str of
        Left e -> error (show e)
        Right cd ->
            let
                (s,ld) = cconvDefs 0 cd
                ((s',_),pd) = partialsDef (mkGlobalMap ld) (s,mempty) ld
                (ad,_) = anfifyDefs s' pd
                c = cgen ad
            in writeFile (root ++ file ++ ".c") c

main :: IO ()
main = do
    (root:fs) <- getArgs
    mapM_ (buildFile root) fs