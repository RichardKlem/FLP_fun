--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022


--1. Parse args
--	1.1. parameters -i -1 -2
--	1.2. file OR stdin - parse lines into data structure
--2. convert RightLinGr to ReGr
--3. convert ReGr to NFA

module Main
    ( main
    ) where

import System.Environment
import System.Exit

data RLG = RLG {  nonterminals :: [Char],
                  terminals :: [Char],
                  startSymbol  :: Char,
                  rules :: [(Char,[Char])] }

data NFA = NFA {  states :: [Int],
                    inputAlphabet :: [Char],
                    transitionFunction  :: [(Int,Char,Int)],
                    initialState :: Int,
                    finalStates :: [Int] }

main :: IO ()
main = getArgs >>= parse

parse ["-i":xs] = info >> exit
parse ["-1":xs] = regularGrammar >> exit
parse ["-2":xs] = finiteAutomaton >> exit
parse ["-h":xs] = usage >> exit
parse [] = "Too few arguments." ++ usage >> exitError
parse _ = "Invalid usage." ++ usage >> exitError
--parse fs     = concat `fmap` mapM readFile fs

info = "Info"
regularGrammar = "regularGrammar"
finiteAutomaton = "finiteAutomaton"

usage   = putStrLn "Usage: flp-fun [-i12] file OR <stdin>"
exit :: IO a
exit = exitSuccess
exitError :: IO a
exitError = exitWith (ExitFailure 1)
