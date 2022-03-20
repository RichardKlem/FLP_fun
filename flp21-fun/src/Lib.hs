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
    ( rlg2nfa
    ) where

import System.Environment
import System.Exit
import System.IO (putStrLn, stderr)
import Data.List (nub, intersperse, intercalate)
import Text.Show.Functions

-- data RLG = RLG {  nonterminals :: [String], terminals :: [String], startSymbol :: Char, rules :: [(Char, String)] }
type Rules = [String]
type StartSymbol = Char
type Nonterminals = [String]
data RLG = RLG {  nonterminals :: [String], terminals :: [String], startSymbol :: Char, rules :: [String] }
data NFA = NFA {  states :: [Int], inputAlphabet :: String, transitionFunction :: [(Int,Char,Int)],
  initialState :: Int, finalStates :: [Int] }

newParse :: [String] -> String
newParse ("-i":x:xs) = x -- showRLG parseInput

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

showRLG :: RLG -> String
showRLG a = concat (concat [
    [concat $ intersperse "," (nonterminals a) ++ ["\n"]],
    [concat $ intersperse "," (terminals a) ++ ["\n"]],
    [concat $ [startSymbol a] : ["\n"]],
    [unlines $ rules a]
    ]
  )

-- TODO Dokoncit tuto funkci.
removeInvalidRules :: Rules -> Rules
removeInvalidRules _ = ["me"]


{-___ MAIN ___-}
rlg2nfa :: IO ()

-- rlg2nfa = getArgs >>= parse
rlg2nfa = do
  args <- getArgs
  let fileName = newParse args
  fs <- readFile fileName
  let nonterminalsString:terminalsString:startSymbolString:rulesString = lines fs
  let nonterminals = nub (wordsWhen (==',') nonterminalsString)
  let terminals = nub (wordsWhen (==',') terminalsString)
  let startSymbol = head $ head $ nub (wordsWhen (==',') startSymbolString)
  let rules = nub $ filter (not . null) rulesString
  let rlg = RLG nonterminals terminals startSymbol rules

  putStr $ showRLG rlg

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
