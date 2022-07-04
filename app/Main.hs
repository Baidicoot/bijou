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
import Typecheck
import Rename

import System.Environment

buildFile :: String -> String -> IO ()
buildFile root file = do
    str <- readFile (root ++ file)
    case parseTLDefs (root ++ file) str of
        Left e -> error (show e)
        Right cd ->
            let
                rd = renameTL cd
                (s,ld) = cconvDefs mempty 0 rd
                ((s',_),pd) = partialsDef (mkGlobalMap ld) (s,mempty) ld
                (ad,s'') = anfifyDefs s' pd
                c = cgen ad
            in do
                writeFile (root ++ file ++ ".c") c
                print rd
                print (runInferDefs (s'',mempty) mempty rd)

main :: IO ()
main = do
    (root:fs) <- getArgs
    mapM_ (buildFile root) fs