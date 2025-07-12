module Main where

import qualified Data.Map as M
import Data.List (intercalate)

data Trie = Prefix String [Trie] | Leaf String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  states <- lines <$> readFile "state"
  putStrLn $ renderMany $ toTries states
  pure ()

toTries :: [String] -> [Trie]
toTries [x] = [Leaf x]
toTries xs =
  map toTrie $
    toItemsAndSuffixes xs

toItemsAndSuffixes :: [String] -> [(Char, [String])]
toItemsAndSuffixes xs =
  M.toList $
    M.fromListWith (++) $
      map (\state -> (head state, [tail state])) xs

toTrie :: (Char, [String]) -> Trie
toTrie (prefixChar, strs) =
  let tries = toTries strs
  in
  case tries of
    [Prefix cs subtries] -> Prefix (prefixChar : cs) subtries
    [Leaf str] -> Leaf (prefixChar : str)
    _ -> Prefix [prefixChar] tries

renderOne :: Trie -> String
renderOne (Leaf str) = str
renderOne (Prefix cs tries) = cs ++ renderMany tries

renderMany :: [Trie] -> String
renderMany tries = "(" ++ intercalate "|" (map renderOne tries) ++ ")"
