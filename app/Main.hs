{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Datatypes.Core

import ClosureConv
import PartialApp
import Parser
import ANFify
import Backend
import Typecheck
import Desugar

import System.Environment

buildFile :: String -> String -> IO ()
buildFile root file = do
    str <- readFile (root ++ file)
    case parseTL (root ++ file) str of
        Left e -> error (show e)
        Right tl -> do
            print tl
            print (desugarMod mempty tl)
            let (Right m) = desugarMod mempty tl
            let (ld,s) = cconvDefs mempty 0 m
            print ld
            let (s',pd) = partialsMod s mempty m ld
            print pd
            let (ad,s'') = anfifyDefs s' pd
            print ad
            let c = cgen mempty m ad
            let h = hgen mempty m ad
            case runInferMod s'' mempty m of
                (Right (t,_),_) -> do
                    mapM_ (\(n,p) -> putStrLn $ show n ++ " :: " ++ show p) t
                    writeFile (root ++ file ++ ".c") c
                    writeFile (root ++ file ++ ".h") h
                (Left e,d) -> mapM putStrLn d >> print e

main :: IO ()
main = do
    (root:fs) <- getArgs
    mapM_ (buildFile root) fs