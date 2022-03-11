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
import System.IO (putStrLn, stderr)
import Data.List (nub)

data RLG = RLG {  nonterminals :: String, terminals :: String, startSymbol  :: Char, rules :: [(Char, String)] }
data NFA = NFA {  states :: [Int], inputAlphabet :: String, transitionFunction  :: [(Int,Char,Int)],
  initialState :: Int, finalStates :: [Int] }

newParse :: [String] -> String
newParse ("-i":x:xs) = x

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


algo :: IO ()
-- algo = getArgs >>= parse
algo = do
  args <- getArgs
  let fileName = newParse args
  fs <- readFile fileName
  let nonterminals:terminals:startSymbol:rules = lines fs
  print $ nub (wordsWhen (==',') nonterminals)
  putStrLn terminals
  putStrLn startSymbol
  print rules
--
-- parse :: [String] -> IO ()
-- parse ("-i":xs) = putStr (info ++ ":" ++ show xs) >> exit
-- parse ("-1":xs) = putStr regularGrammar >> exit
-- parse ("-2":xs) = putStr finiteAutomaton >> exit
-- parse ("-h":_) = putStr usage >> exit
-- parse [] = putStr ("Too few arguments. Here is the help:\n" ++ usage) >> exitError
-- parse _ = putStr ("Invalid usage. Here is the help:\n" ++ usage) >> exitError
--
-- -- myReadFile fs = concat `fmap` mapM readFile fs
--
--
-- info :: String
-- info = "Info"
-- regularGrammar :: String
-- regularGrammar = "regularGrammar"
-- finiteAutomaton :: String
-- finiteAutomaton = "finiteAutomaton"
--
-- usage :: String
-- usage =  "  Usage: flp-fun [-i12] file OR <stdin>\n"
-- exit :: IO a
-- exit = exitSuccess
-- exitError :: IO a
-- exitError = exitWith (ExitFailure 1)
