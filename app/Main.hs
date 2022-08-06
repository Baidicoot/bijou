{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Control.Monad
--import Build
{-
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' ->
        let (w, s'') = break p s'
        in w : wordsWhen p s''

main :: IO ()
main = do
    (root:build:fs) <- getArgs
    let ps = fmap (wordsWhen (=='.')) fs
    ms <- foldM (\ms p -> fmap ((:ms) . (,) p) (buildModule root build ms p)) [] ps
    pure ()
-}

main :: IO ()
main = pure ()