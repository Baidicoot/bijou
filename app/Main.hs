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
        Right (st,exp,cd) ->
            let
                rd = renameTL exp cd
                (s,ld) = cconvDefs mempty 0 rd
                ((s',_),pd) = partialsDef (mkGlobalMap ld) (s,mempty) ld
                (ad,s'') = anfifyDefs s' pd
                c = cgen st ad
                h = hgen ad
            in case runInferRec (s'',mempty) mempty (fmap desugarDef rd) of
                Right (t,_) -> do
                    print cd
                    mapM_ (\(n,p) -> putStrLn $ show n ++ " :: " ++ show p) t
                    writeFile (root ++ file ++ ".c") c
                    writeFile (root ++ file ++ ".h") h
                Left e -> print e

main :: IO ()
main = do
    (root:fs) <- getArgs
    mapM_ (buildFile root) fs