--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022

module Types where

import qualified Data.Map as Map (Map)

type Nonterminals = [String]
type Terminals = [String]
type StartSymbol = Char
type Rule = (String, String)
type Rules = [Rule]
data Grammar = Grammar {nonterminals :: Nonterminals, terminals :: Terminals, startSymbol :: StartSymbol, ruleSet :: Rules}
  deriving Show

type StatesDict = Map.Map String Int
type Dict = Map.Map Char Int

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
