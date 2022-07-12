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
        Right tl ->
            let
                (Right m) = desugarMod mempty tl
                (ld,s) = cconvDefs mempty 0 m
                (s',pd) = partialsMod s mempty m ld
                (ad,s'') = anfifyDefs s' pd
                c = cgen mempty m ad
                h = hgen mempty m ad
            in case runInferRec (s'',mempty) (mconcat (fmap consTypes dat)) _ of
                Right (t,_) -> do
                    mapM_ (\(n,p) -> putStrLn $ show n ++ " :: " ++ show p) t
                    writeFile (root ++ file ++ ".c") c
                    writeFile (root ++ file ++ ".h") h
                Left e -> print e

main :: IO ()
main = do
    (root:fs) <- getArgs
    mapM_ (buildFile root) fs