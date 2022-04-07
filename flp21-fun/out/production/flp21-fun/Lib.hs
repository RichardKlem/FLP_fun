--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022

module Lib
  ( mainFunc,
  )
where

import Data.List (intersperse, elemIndex, intercalate)
import Data.Containers.ListUtils (nubOrd)
import Data.Map (fromList, lookup, empty, alter, Map)
import Prelude hiding (lookup)
import Data.Maybe (fromJust, isNothing)
import System.Environment
import GHC.Conc (pseq)
import Control.Monad (when)
import Utils
-- USEFUL STUFF --
  -- insert or update => alter (\_ -> Just <val>) <key> Dict
  -- insert or update => alter (\_ -> Just <val>) <key> Dict

-- data Grammar = Grammar {  nonterminals :: [String], terminals :: [String], startSymbol :: Char, rules :: [(Char, String)] }
type Nonterminals = [String]
type Terminals = [String]
type StartSymbol = Char
type Rule = (String, String)
type Rules = [Rule]
data Grammar = Grammar {nonterminals :: Nonterminals, terminals :: Terminals, startSymbol :: StartSymbol, rules :: Rules}
  deriving Show

type StatesDict = Map String Int
type Dict = Map Char Int

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



showRLG :: Grammar -> String
showRLG a =
  concat
    ( concat
        [ [concat $ intersperse "," (nonterminals a) ++ ["\n"]],
          [concat $ intersperse "," (terminals a) ++ ["\n"]],
          [concat $ [startSymbol a] : ["\n"]],
          [unlines $ map (\(left, right) -> left ++ "->" ++ right) (rules a)]
        ]
    )

showRRG :: Grammar -> String
showRRG a = "Right Regular Grammar:\n" ++ showRLG a

showNFA :: NFA -> String
showNFA a =
  "Non-deterministic Finite Automaton:\n" ++ concat
    ( concat
        [ [concat $ intersperse "," (map show (states a)) ++ ["\n"]],
          [inputAlphabet a ++ "\n"],
          [show (initialState a) ++ "\n"],
          [concat $ intersperse "," (map show (finalStates a)) ++ ["\n"]],
          [unlines (map (\(q, c, p) -> intercalate "," [show q, [c], show p]) (transitionFunction a))]
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


-- Convert rule from Grammar to type 3 grammar (RG).
-- Zde se jiz muzu psolehnout na korektni tvar pravidel.
--convertRule r = if ((last (snd r) `elem` ['A' .. 'Z']) && length (snd r) == 2 ) || snd r == "#" then [r] else [("","")]
--convertRuleRRG :: Rule -> Rules -- TODO
--convertRuleRRG rule = (\(l,r) -> if (((length r) <= 2) && (['A'..'Z'] `elem` r)) then [(l,r)] else if (not (['A'..'Z'] `elem` r)) then generateRules (l,r) 1 else [(l,r)]) rule
--convertRuleRRG rule = (\(l,r) -> if (((length r) <= 2) && ( any (`elem` ['A'..'Z'])) r) then [(l,r)] else if (not ( any (`elem` ['A'..'Z']) r)) then generateRules [(l,r)] 1 else [(l,"!!!")]) rule
convertRuleRRG :: Rule -> Dict -> Rules -> (Dict, Rules)
convertRuleRRG (l,r) counter rules =   
    if (r == "#") || ((length r == 2) && any (`elem` ['A'..'Z']) r) then (counter, [(l,r)])  -- ("X", "#"), pozdeji se prevede X na koncovy stav a pravidlo se odstrani. -- ("X", "xY") 
    else generateRules (l,r) counter rules
 

-- TODO dodelat tyto zpicene hacky na nove neterminaly
--generateRules :: Rules -> Int -> Rules
--generateRules [(l, "#")] _ = [(l, "#")]
--generateRules ((l,r)) i = [(l, head r ++ l ++ show i)] ++ (generateRules r (i + 1)) -- A->aa | A->abacracga

--generateRules [(l, _)] _ = [(l, "#")]

getFromCounter :: Char -> Dict -> Int
getFromCounter k c = if isNothing (lookup k c) then 0 else fromJust(lookup k c)

incrementCounter :: Char -> Dict -> Dict
incrementCounter k c = alter (\_ -> Just (if isNothing (lookup k c) then 0 else fromJust(lookup k c) + 1)) k c
-- Increment nonterminal's counter and return new appropriate nonterminal.
getNextNonterminal :: Char -> Dict -> String
getNextNonterminal nt c = nt:show (getFromCounter nt c)

generateRules :: Rule -> Dict -> Rules -> (Dict, Rules)
generateRules (l, [r]) c rules = ( incrementCounter (head l) c, rules ++ [(l, r:getNextNonterminal (head l) c)] ++ [(getNextNonterminal (head l) c, "#")])  
generateRules (l, r:rs) c rules = 
  if head rs `elem` ['A'..'Z'] then (incrementCounter (head l) c, (l,r:rs):rules) 
  else generateRules (getNextNonterminal (head l) c, rs) (incrementCounter (head l) c) ((l,r:getNextNonterminal (head l) c):rules)  

getFinalStates :: Grammar -> StatesDict -> FinalStates
getFinalStates rrg dict = map (\(l,_) -> fromJust(lookup l dict)) (filter (\(_,r) -> r == "#") (rules rrg))

getTransitionFunction :: Rules -> StatesDict -> TransitionFunction  -- I am not able to make this type work.
getTransitionFunction rs dict = map (\(l, r) -> (fromJust(lookup l dict), head r, fromJust(lookup (tail r) dict))) (filter (\(_,r) -> r /= "#") rs)


convertToRRG :: Grammar -> IO Grammar
convertToRRG rlg = do
  let counterDict = fromList(zip ['A'..'Z'] (replicate 26 0))
  --  fce (l,r) = if (any (`elem` ['A'..'Z'] (dropWhile (`elem` ['a'..'z']) r)))
  -- TODO predavani jednoho counterDict do vsech pravidel. Je to potreba vlastne?
  -- TODO osetrit chybejici (ne)terminaly nebo je dopocitat z pravidel.
  let rulesRRG = concatMap (\rule -> snd (convertRuleRRG rule counterDict [])) (rules rlg)
  
  let nonterminalsRRG = nubOrd (nonterminals rlg ++ map fst rulesRRG)
  let terminalsRRG = filter (/= "#") $ nubOrd (terminals rlg ++ map (\r -> [head(snd r)]) rulesRRG)
  let grammar = Grammar {
                  nonterminals = nonterminalsRRG,
                  terminals = terminalsRRG,
                  startSymbol = startSymbol rlg,
                  rules = rulesRRG
                  }
  print grammar  -- DEBUG
  return grammar

convertToNFA :: Grammar -> IO NFA
convertToNFA rrg = do
  let dict = fromList (zip (nonterminals rrg) [0..length (nonterminals rrg) - 1])
  return NFA { states = [0.. length (nonterminals rrg) - 1],  -- (map (\nt -> fromJust (elemIndex nt (map head (nonterminals rrg)))) (map head (nonterminals rrg))),
               inputAlphabet = concat (terminals rrg),
               initialState = fromJust (elemIndex (startSymbol rrg) (map head (nonterminals rrg))),
               finalStates = getFinalStates rrg dict,
               transitionFunction = getTransitionFunction (rules rrg) dict}



{-___ MAIN ___-}
mainFunc :: IO ()
-- rlg2nfa = getArgs >>= parse
mainFunc = do
    args <- getArgs
    let (variant, fileName) = newParse args
    input <- getInput fileName
    let n = length input  --n `pseq` (something) je hack na vyckani z evaluaci stdin az do doby kdy je poslan eof a ne driv.

    case variant of
      0 -> getRLG input >>= (n `pseq` (putStr . showRLG))
      1 -> getRLG input >>= convertToRRG >>= (n `pseq` (putStr . showRRG))
      2 -> getRLG input >>= convertToRRG >>= convertToNFA >>= (n `pseq` (putStr . showNFA))
      _ -> error "Neplatne argumenty."


getRLG :: Monad m => [String] -> m Grammar
getRLG input = do
  let userInput = input
  when (length userInput < 4) $ error "Nekompletni vstup."
  let nonterminalsString : terminalsString : startSymbolString : rulesString = input
  let nonterminals = map ((\a -> if length a == 1 && head a `elem` ['A' .. 'Z'] then a else error "Neterminal muze byt pouze jedno z velkych pismen 'A' az 'Z'") . skipSpaces) (nubOrd (wordsWhen (== ',') nonterminalsString))
  let terminals = map skipSpaces (nubOrd (wordsWhen (== ',') terminalsString))
  let startSymbol = if length (skipSpaces startSymbolString) == 1 then head $ skipSpaces startSymbolString else error "Startovaci symbol musi byt jeden znak na samostatnem radku."
--  let rules = filter (/= ("", "")) (map checkIfValidRule (nubOrd $ filter (not . null) rulesString))
  let rules = concatMap ((: []) . checkIfValidRule . skipSpaces) (nubOrd $ filter (not . null) rulesString)
  let rlg = Grammar nonterminals terminals startSymbol rules

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
