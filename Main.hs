{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map as M
import Data.List (intercalate)

data Trie = Prefix String [Trie] | Leaf String
  deriving (Show, Eq, Ord)

data RenderOptions = RenderOptions
  { mode :: RenderMode
  , redRGB, whiteRGB, blueRGB :: (Int, Int, Int)
  , maxWidth :: Maybe Int
  }
  deriving (Show, Eq, Ord)

data RenderMode = HTML | ANSI
  deriving (Show, Eq, Ord)

main = allAmericanRegex HTML

allAmericanRegex :: RenderMode -> IO ()
allAmericanRegex mode = do
  states <- lines <$> readFile "state"
  let options = RenderOptions
        { mode = mode
        , blueRGB = (60,59,110)
        , redRGB = (178,34,52)
        , whiteRGB = (255,255,255)
        , maxWidth = Just 41
        }
  putStrLn $ renderTop options $ toTries states
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
    [Prefix prefix subtries] -> Prefix (prefixChar : prefix) subtries
    [Leaf str] -> Leaf (prefixChar : str)
    _ -> Prefix [prefixChar] tries

renderTop :: RenderOptions -> [Trie] -> String
renderTop opts@RenderOptions {..} tries =
  case mode of
    HTML ->
      let grey = (120, 120, 120)
          header = renderRGB HTML grey "# All-American Regex"
          slash = renderRGB HTML grey "/"
      in
      unlines
        [ "<style>body { background-color: black; } #regex { font-family: monospace; }</style>\n"
        , "<pre id=\"regex\">"
        , joinBookends header
        , intercalate "<br/>" $
            map joinBookends $
              maybe pure chunks maxWidth $
                slash ++ renderMany opts tries ++ slash
        , "</pre>"
        ]
    _ ->
      unlines $
        map joinBookends $
          maybe pure chunks maxWidth $
            renderMany opts tries

chunks :: Int -> [a] -> [[a]]
chunks size [] = []
chunks size cs =
  let (chunk, rest) = splitAt size cs
  in
  chunk : chunks size rest

renderOne :: RenderOptions -> Trie -> [(String, Char, String)]
renderOne opts@RenderOptions {..} (Leaf str) =
  renderRGB mode whiteRGB str
renderOne opts@RenderOptions {..} (Prefix prefix tries) =
  renderRGB mode whiteRGB prefix ++ renderMany opts tries

joinBookends :: [(String, Char, String)] -> String
joinBookends = concatMap $ \(pre, c, post) -> pre ++ [c] ++ post

renderMany :: RenderOptions -> [Trie] -> [(String, Char, String)]
renderMany opts@RenderOptions {..} tries =
  concat
    [ renderRGB mode blueRGB "("
    , intercalate (renderRGB mode redRGB "|") $
        map (renderOne opts) tries
    , renderRGB mode blueRGB ")"
    ]

renderRGB :: RenderMode -> (Int, Int, Int) -> String -> [(String, Char, String)]
renderRGB mode color str =
  let (pre, post) = rgbBookends mode color
  in
  map (pre,,post) str

rgbBookends :: RenderMode -> (Int, Int, Int) -> (String, String)
rgbBookends ANSI (r, g, b) =
  ( concat
      [ "\ESC[38;2;"
      , intercalate ";" $ map show [r, g, b]
      , "m"
      ]
  , "\ESC[0m"
  )
rgbBookends HTML (r, g, b) =
  ( concat
      [ "<span style=\"color: rgb("
      , intercalate "," $ map show [r, g, b]
      , ")\">"
      ]
  , "</span>"
  )
