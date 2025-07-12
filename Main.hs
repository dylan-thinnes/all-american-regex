{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map as M
import Data.List (intercalate)

data Trie = Prefix String [Trie] | Leaf String
  deriving (Show, Eq, Ord)

data RenderOptions = RenderOptions
  { mode :: RenderMode
  , redRGB, whiteRGB, blueRGB :: (Int, Int, Int)
  }
  deriving (Show, Eq, Ord)

data RenderMode = HTML | ANSI
  deriving (Show, Eq, Ord)

main = allAmericanRegex ANSI

allAmericanRegex :: RenderMode -> IO ()
allAmericanRegex mode = do
  states <- lines <$> readFile "state"
  let options = RenderOptions
        { mode = mode
        , blueRGB = (60,59,110)
        , redRGB = (178,34,52)
        , whiteRGB = (255,255,255)
        }
  putStrLn $ renderMany options $ toTries states
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

renderOne :: RenderOptions -> Trie -> String
renderOne opts (Leaf str) = str
renderOne opts (Prefix cs tries) = cs ++ renderMany opts tries

renderMany :: RenderOptions -> [Trie] -> String
renderMany opts@RenderOptions {..} tries =
  case mode of
    HTML -> error "HTML"
    ANSI ->
      renderRGBAnsi blueRGB "(" ++ intercalate (renderRGBAnsi redRGB "|") (map (renderOne opts) tries) ++ renderRGBAnsi blueRGB ")"

renderRGBAnsi :: (Int, Int, Int) -> String -> String
renderRGBAnsi (r, g, b) str =
  "\ESC[38;2;" ++ intercalate ";" (map show [r, g, b]) ++ "m" ++ str ++ "\ESC[0m"
