--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022

module Lib
  ( mainFunc,
  )
where

import Data.List (nub, intersperse, elemIndex, intercalate)
import qualified Data.Map as Map (fromList, lookup, alter)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import System.Environment
import GHC.Conc (pseq)
import Utils
import Types


{-___ Input parsing functions ___-}
getInput :: [FilePath] -> IO [String]
getInput [] = fmap lines getContents
getInput [x] = fmap lines (readFile x)
getInput _ = error "Invalid arguments."

parseInput :: [String] -> (Int, [String])
parseInput ("-i" : x) = (0, x)
parseInput ("-1" : x) = (1, x)
parseInput ("-2" : x) = (2, x)
parseInput ("-h" : x) = (3, x)
parseInput _ = error "Invalid arguments."

isInputEmpty :: [String] -> Bool
isInputEmpty input = [] `elem` take 4 input 

{-___ RLG related functions ___-}
-- input is list of lines splited by newline from user input
getRLG :: [String] -> Grammar
getRLG input =
  if length input < 4 || isInputEmpty input then error "The input is incomplete. Please check the input carefully."
  else
    (\(nonterminalsString:terminalsString:startSymbolString:rulesString) ->
      Grammar {
        nonterminals = map ((\a -> if length a == 1 && head a `elem` ['A' .. 'Z'] then a else error "Nonterminal can be only one character upper case letter from set ['A'..'Z'].") . skipSpaces) (nub (wordsWhen (== ',') nonterminalsString)),
        terminals = map skipSpaces (nub (wordsWhen (== ',') terminalsString)),
        startSymbol = if length (skipSpaces startSymbolString) == 1 then head $ skipSpaces startSymbolString else error "Start symbol must be only one character upper case letter from set ['A'..'Z'] on separated line.",
        ruleSet = concatMap ((: []) . checkIfValidRule . skipSpaces) (nub $ filter (not . null) rulesString)
        }
      ) input

showRLG :: Grammar -> String
showRLG a =
  concat
    ( concat
        [ [concat $ intersperse "," (nonterminals a) ++ ["\n"]],
          [concat $ intersperse "," (terminals a) ++ ["\n"]],
          [concat $ [startSymbol a] : ["\n"]],
          [unlines $ map (\(left, right) -> left ++ "->" ++ right) (ruleSet a)]
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
  then ([left], right) else error ("Wrong syntax of rule: " ++ left:y:z:right)
checkIfValidRule badRule = error ("Wrong syntax of rule: " ++ badRule)


{-___ RRG related functions ___-}
-- Convert RRG type rule to RRG type rule.
-- I can already rely on correct rule format here.
convertRuleRRG :: Rule -> Dict -> Rules -> (Dict, Rules)
convertRuleRRG (l,r) counter rules =
    if (r == "#") || ((length r == 2) && any (`elem` ['A'..'Z']) r) then (counter, [(l,r)])
    else generateRRGRules (l,r) counter rules

-- Returns count of nonterminal appearences.
getFromCounter :: Char -> Dict -> Int
getFromCounter k c = fromMaybe 0 (Map.lookup k c)

-- Increments count of nonterminal appearences.
incrementCounter :: Char -> Dict -> Dict
incrementCounter k c = Map.alter (\_ -> Just (if isNothing (Map.lookup k c) then 0 else fromJust(Map.lookup k c) + 1)) k c

-- Increment nonterminal's counter and return new appropriate nonterminal.
getNextNonterminal :: Char -> Dict -> String
getNextNonterminal nt c = nt:show (getFromCounter nt c)

generateRRGRules :: Rule -> Dict -> Rules -> (Dict, Rules)
generateRRGRules (l, [r]) c rules = ( incrementCounter (head l) c, rules ++ [(l, r:getNextNonterminal (head l) c)] ++ [(getNextNonterminal (head l) c, "#")])
generateRRGRules (l, r:rs) c rules =
  if head rs `elem` ['A'..'Z'] then (incrementCounter (head l) c, (l,r:rs):rules)
  else generateRRGRules (getNextNonterminal (head l) c, rs) (incrementCounter (head l) c) ((l,r:getNextNonterminal (head l) c):rules)
-- Only for completely exhaustive patterns. The format of input is checked before.
generateRRGRules _ c rules = (c, rules) 

-- Convert Right linear grammar to type 3 grammar (Regular grammar).
convertToRRG :: Grammar -> Grammar
convertToRRG rlg =
  Grammar {
                  nonterminals = nub (nonterminals rlg ++ map fst rulesRRG),
                  terminals = filter (/= "#") $ nub (terminals rlg ++ map (\r -> [head(snd r)]) rulesRRG),
                  startSymbol = startSymbol rlg,
                  ruleSet = rulesRRG
                  }
                  where
                    counterDict = Map.fromList(zip ['A'..'Z'] (replicate 26 0))
                    rulesRRG = concatMap (\rule -> snd (convertRuleRRG rule counterDict [])) (ruleSet rlg)

showRRG :: Grammar -> String
showRRG = showRLG


{-___ NFA related functions ___-}
getFinalStates :: Grammar -> StatesDict -> FinalStates
getFinalStates rrg dict = map (\(l,_) -> fromJust(Map.lookup l dict)) (filter (\(_,r) -> r == "#") (ruleSet rrg))

getTransitionFunction :: Rules -> StatesDict -> TransitionFunction
getTransitionFunction rs dict = map (\(l, r) -> (fromJust(Map.lookup l dict), head r, fromJust(Map.lookup (tail r) dict))) (filter (\(_,r) -> r /= "#") rs)

convertToNFA :: Grammar -> NFA
convertToNFA rrg =
  NFA { states = [0.. length (nonterminals rrg) - 1],
               inputAlphabet = concat (terminals rrg),
               initialState = fromJust (elemIndex (startSymbol rrg) (map head (nonterminals rrg))),
               finalStates = getFinalStates rrg dict,
               transitionFunction = getTransitionFunction (ruleSet rrg) dict}
               where
                 dict = Map.fromList (zip (nonterminals rrg) [0..length (nonterminals rrg) - 1])

showNFA :: NFA -> String
showNFA a =
 (concat . concat)
       [[concat $ intersperse "," (map show (states a)) ++ ["\n"]],
         [inputAlphabet a ++ "\n"],
         [show (initialState a) ++ "\n"],
         [concat $ intersperse "," (map show (finalStates a)) ++ ["\n"]],
         [unlines (map (\(q, c, p) -> intercalate "," [show q, [c], show p]) (transitionFunction a))]
       ]


{-___ MAIN ___-}
mainFunc :: IO ()
mainFunc = do
    args <- getArgs
    let (variant, fileName) = parseInput args
    input <- getInput fileName
    let n = length input
    {-n `pseq` <something> is a way to ensure the evaluation waits for whole
    user input. It is needed for stdin input loading.-}

    case variant of
      0 -> ((n `pseq` (putStr . showRLG)) . getRLG) input
      1 -> ((n `pseq` (putStr . showRRG)) . convertToRRG . getRLG) input
      2 -> ((n `pseq` (putStr . showNFA)) . convertToNFA . convertToRRG . getRLG) input
      3 -> putStrLn help
      _ -> error "Invalid arguments."
