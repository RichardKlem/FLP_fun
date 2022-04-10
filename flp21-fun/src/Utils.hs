--Project name: FLP 2021/2022 – funkcionální projekt: Haskell
--Login: xklemr00
--Author: Richard Klem
--Year: 2022

module Utils where

-- https://stackoverflow.com/a/68056547
skipSpaces :: String -> String
skipSpaces [] = []
skipSpaces (' ':t) = skipSpaces t
skipSpaces (h:t) = h : skipSpaces t

-- https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

upperCount :: String -> Int
upperCount string = length (filter (\c -> c `elem` ['A'..'Z']) string)

help :: String
help = "Run the programme with arguments: ./flp-fun [-i|-1|-2|-h] <filename>|<stdin> \n\
       \Choose one of the switches, only one at the time is allowed, no overriding is permited. \n\
       \Then choose if you want provide input in file or you will write the input at stdin. \n\
       \When stdin input is chosen, end your entry by ^Z signal on Windows or ^D signal on Linux."