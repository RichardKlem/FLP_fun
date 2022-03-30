--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022

module Lib
  ( rlg2nfa,
  )
where

import Data.List (intersperse, nub, elemIndex, intercalate)
import Data.Map (fromList, lookup)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import System.Environment
import GHC.Conc (pseq)
import Utils

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
type InitialState = Int
type FinalStates = [Int]
type Transition = (Int, Char, Int)
type TransitionFunction = [Transition]

data NFA = NFA
  { states :: States,
    inputAlphabet :: InputAlphabet,
    initialState :: InitialState,
    finalStates :: FinalStates,
    transitionFunction :: TransitionFunction
  }

getInput :: [FilePath] -> IO [String]
getInput [] = lines <$> getContents
getInput [x] = lines <$> readFile x
getInput _ = error "Invalid arguments."

newParse :: [String] -> (Int, [String])
newParse ("-i" : x) = (0, x)
newParse ("-1" : x) = (1, x)
newParse ("-2" : x) = (2, x)
newParse _ = error "Neplatne argumenty."



showRLG :: RLG -> String
showRLG a =
  concat
    ( concat
        [ [concat $ intersperse "," (nonterminals a) ++ ["\n"]],
          [concat $ intersperse "," (terminals a) ++ ["\n"]],
          [concat $ [startSymbol a] : ["\n"]],
          [unlines $ map (\(left, right) -> left ++ "->" ++ right) (rules a)]
        ]
    )

showRRG :: RRG -> String
showRRG a = "Right Regular Grammar:\n" ++ showRLG a

showNFA :: NFA -> String
showNFA a =
  "Non-deterministic Finite Automaton:\n" ++ concat
    ( concat
        [ [concat $ intersperse "," (map show (states a)) ++ ["\n"]],
          [inputAlphabet a ++ "\n"],
          [show (initialState a) ++ "\n"],
          [concat $ intersperse "," (map show (finalStates a)) ++ ["\n"]],
          [unlines $ (map (\(q, c, p) -> intercalate "," [show q, [c], show p]) (transitionFunction a))]
        ]
    )

checkIfValidRule :: String -> Rule
checkIfValidRule (left : y : z : right) =
  if
    left `elem` ['A'..'Z'] &&
    [y,z] == "->" &&
    all (\r -> r `elem` ['A'..'Z'] || r `elem` ['a'..'z'] || r == '#') right &&
    (
      ((upperCount right == 1) && ((last right `elem` ['A'..'Z']) && length right > 1)) ||
      ((upperCount right == 0) && ((last right `elem` ['a'..'z']) || (last right == '#')))
    )
  then ([left], right) else error ("Spatna syntaxe pravidla: " ++ left:y:z:right)
checkIfValidRule rule = error ("Spatna syntaxe pravidla: " ++ rule)

-- AA->Aa  //INVALID
-- A->Aa   //INVALID
-- A->AA   //INVALID
-- A->aAa  //INVALID
-- A->aAaA //INVALID
-- A->aa   //VALID ==> A->aA1, A1->aA2, A2->#  // zde jediny pripad, kdy generuji NonTer->#
-- A->aB   //VALID ==> B-># se negeneruje, musi byt uz v PLG
-- A->abA  //VALID ==> A->aA1, A1->bA   //musi byt definovano A-># od uzivatele stejne jako nahore


-- Convert rule from RLG to type 3 grammar (RG).
-- Zde se jiz muzu psolehnout na korektni tvar pravidel.
convertRuleRRG :: Rule -> Rules
--convertRule r = if ((last (snd r) `elem` ['A' .. 'Z']) && length (snd r) == 2 ) || snd r == "#" then [r] else [("","")]
convertRuleRRG r = [r] -- TODO

-- getTransitionFunction :: Rules -> TransitionFunction
getTransitionFunction rs dict = map (\(l, r) -> if r == "#" then (fromJust(lookup l dict), '#', fromJust(lookup l dict)) else (fromJust(lookup l dict), head r, fromJust(lookup (tail r) dict))) rs

-- convertToRRG :: RLG -> RRG
convertToRRG = return -- TODO

--convertToNFA :: RRG -> NFA -- TODO
convertToNFA rrg = do
  let dict = fromList (zip (nonterminals rrg) [0..length (nonterminals rrg) - 1])
  return NFA { states = [0.. length (nonterminals rrg) - 1],--(map (\nt -> fromJust (elemIndex nt (map head (nonterminals rrg)))) (map head (nonterminals rrg))),
               inputAlphabet = concat (terminals rrg),
               initialState = fromJust (elemIndex (startSymbol rrg) (map head (nonterminals rrg))),
               finalStates = [0.. length (nonterminals rrg) - 1],
               transitionFunction = getTransitionFunction (rules rrg) dict}

{-___ MAIN ___-}
rlg2nfa :: IO ()
-- rlg2nfa = getArgs >>= parse
rlg2nfa = do
    args <- getArgs
    let (variant, fileName) = newParse args
    input <- getInput fileName
    let n = length input  --n `pseq` (something) je hack na vyckani z evaluaci stdin az do doby kdy je poslan eof a ne driv.

    case variant of
      0 -> getRLG input >>= (n `pseq` (putStr . showRLG))
      1 -> getRLG input >>= convertToRRG >>= (n `pseq` (putStr . showRRG))
      2 -> getRLG input >>= convertToRRG >>= convertToNFA >>= (n `pseq` (putStr . showNFA))
      _ -> error "Neplatne argumenty."


getRLG input = do
  let nonterminalsString : terminalsString : startSymbolString : rulesString = input
  let nonterminals = map ((\a -> if length a == 1 && head a `elem` ['A' .. 'Z'] then a else error "Neterminal muze byt pouze jedno z velkych pismen 'A' az 'Z'") . skipSpaces) (nub (wordsWhen (== ',') nonterminalsString))
  let terminals = map skipSpaces (nub (wordsWhen (== ',') terminalsString))
  let startSymbol = if length (skipSpaces startSymbolString) <= 1 then head $ skipSpaces startSymbolString else error "Startovaci symbol musi byt jeden znak na samostatnem radku."
--  let rules = filter (/= ("", "")) (map checkIfValidRule (nub $ filter (not . null) rulesString))
  let rules = concatMap (convertRuleRRG . checkIfValidRule . skipSpaces) (nub $ filter (not . null) rulesString)
  let rlg = RLG nonterminals terminals startSymbol rules

  return rlg


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
