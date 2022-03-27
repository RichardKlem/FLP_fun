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
  ( rlg2nfa,
  )
where

import Data.List (intercalate, intersperse, nub)
import System.Environment
import System.Exit
import System.IO (putStrLn, stderr)
import Text.Show.Functions
import Control.Exception.Base (throw)
import Data.Char (isSpace)

-- data RLG = RLG {  nonterminals :: [String], terminals :: [String], startSymbol :: Char, rules :: [(Char, String)] }
type Nonterminals = [String]
type Terminals = [String]
type StartSymbol = Char
type Rule = (String, String)
type Rules = [Rule]
data RLG = RLG {nonterminals :: Nonterminals, terminals :: Terminals, startSymbol :: StartSymbol, rules :: Rules}
  deriving Show

type RRG = RLG

type States = [Int]
type InputAlphabet = String
type TransitionFunction = [(Int, Char, Int)]
type InitialState = Int
type FinalStates = [Int]

data NFA = NFA
  { states :: States,
    inputAlphabet :: InputAlphabet,
    transitionFunction :: TransitionFunction,
    initialState :: InitialState,
    finalStates :: FinalStates
  }

newParse :: [String] -> String
newParse ("-i" : x : xs) = x -- showRLG parseInput

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

showRLG :: RLG -> String
showRLG a =
  concat
    ( concat
        [ [concat $ intersperse "," (nonterminals a) ++ ["\n"]],
          [concat $ intersperse "," (terminals a) ++ ["\n"]],
          [concat $ [startSymbol a] : ["\n"]],
          [unlines $ map (\(left, right) -> left ++ "->" ++ right) (rules a)] -- Upravit na novy type Rules
        ]
    )

checkIfValidRule :: String -> (String, String)
checkIfValidRule (x : y : z : xs) =
  if [y,z] == "->" &&
    x:"" /= "" &&
    xs /= "" &&
    x `elem` ['A'..'Z'] &&
    all (\x -> x `elem` ['A'..'Z'] || x `elem` ['a'..'z'] || x == '#') xs
  then ([x], xs) else error ("Spatna syntaxe pravidla: " ++ x:y:z:xs)
checkIfValidRule a = error ("Spatna syntaxe pravidla: " ++ a)


-- A->Aa  //INVALID
-- A->aAa //INVALID
-- A->aa  //VALID ==> A->aA1, A1->aA2, A2->#  // zde jediny pripad, kdy generuji NonTer->#
-- A->aB  //VALID ==> B-># se negeneruje, musi byt uz v PLG
-- A->abA //VALID ==> A->aA1, A1->bA   //musi byt definovano A-># od uzivatele stejne jako nahore


-- Convert rule from RLG to type 3 grammar (RG).
convertRule :: Rule -> Rules
convertRule r = [r
  | last (snd r) `elem` ['A' .. 'Z'] || snd r == "#"
  ]

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

{-___ MAIN ___-}
rlg2nfa :: IO ()
-- rlg2nfa = getArgs >>= parse
rlg2nfa = do
  args <- getArgs
  let fileName = newParse args
  fs <- readFile fileName
  let nonterminalsString : terminalsString : startSymbolString : rulesString = lines fs
  let nonterminals = map ((\a -> if length a == 1 && head a `elem` ['A' .. 'Z'] then a else error "Neterminal muze byt pouze jedno z velkych pismen 'A' az 'Z'") . trim) (nub (wordsWhen (== ',') nonterminalsString))
  let terminals = nub (wordsWhen (== ',') terminalsString)
  let startSymbol = if length startSymbolString <= 1 then head startSymbolString else error "Startovaci symbol musi byt jeden znak na samostatnem radku."
--  let rules = filter (/= ("", "")) (map checkIfValidRule (nub $ filter (not . null) rulesString))
  let rules = concatMap (convertRule . checkIfValidRule) (nub $ filter (not . null) rulesString)


  let rlg = RLG nonterminals terminals startSymbol rules

  let rrg = rlg

  putStr $ showRLG rrg

--  print $ filter (/= ("", "")) (map checkIfValidRule rules)
  print rlg

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
