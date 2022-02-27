--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022


--1. Parse args
--	1.1. parameters -i -1 -2
--	1.2. file OR stdin - parse lines into data structure
--2. convert RightLinGr to ReGr
--3. convert ReGr to NFA

module Lib
    ( algo
    ) where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)

data RLG = RLG {  nonterminals :: [Char], terminals :: [Char], startSymbol  :: Char, rules :: [(Char,[Char])] }
data NFA = NFA {  states :: [Int], inputAlphabet :: [Char], transitionFunction  :: [(Int,Char,Int)], 
  initialState :: Int, finalStates :: [Int] }

algo :: IO ()
algo = getArgs >>= parse

parse :: [[Char]] -> IO ()
parse ("-i":xs) = putStr (info ++ ":" ++ show xs) >> exit
parse ("-1":xs) = putStr regularGrammar >> exit
parse ("-2":xs) = putStr finiteAutomaton >> exit
parse ("-h":_) = putStr usage >> exit
parse [] = putStr ("Too few arguments." ++ usage) >> exitError
parse _ = putStr ("Invalid usage." ++ usage) >> exitError
--parse fs     = concat `fmap` mapM readFile fs


info :: [Char]
info = "Info"
regularGrammar :: [Char]
regularGrammar = "regularGrammar"
finiteAutomaton :: [Char]
finiteAutomaton = "finiteAutomaton"

usage :: [Char]
usage =  "Usage: flp-fun [-i12] file OR <stdin>"
exit :: IO a
exit = exitSuccess
exitError :: IO a
exitError = exitWith (ExitFailure 1)
