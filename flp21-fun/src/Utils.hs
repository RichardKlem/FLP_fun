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
upperCount string = length (filter (\r -> (\c -> c `elem` ['A'..'Z']) r) string)