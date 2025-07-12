{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map as M
import Data.List (intercalate)

data Trie = Prefix String [Trie] | Leaf String
  deriving (Show, Eq, Ord)

data RenderOptions = RenderOptions
  { mode :: RenderMode
  , redStyle, whiteStyle, blueStyle :: TextOptions
  , maxWidth :: Maybe Int
  }
  deriving (Show, Eq, Ord)

data TextOptions = TextOptions
  { textStyle :: TextStyle
  , textColor :: (Int, Int, Int)
  }
  deriving (Show, Eq, Ord)

data TextStyle = TSBold | TSItalic | TSPlain
  deriving (Show, Eq, Ord)

data RenderMode = HTML | ANSI | Plain
  deriving (Show, Eq, Ord)

main = allAmericanRegex HTML

allAmericanRegex :: RenderMode -> IO ()
allAmericanRegex mode = do
  states <- lines <$> readFile "state"
  let options = RenderOptions
        { mode = mode
        , blueStyle = TextOptions TSBold (95,92,209)
        , redStyle = TextOptions TSBold (178,34,52)
        , whiteStyle = TextOptions TSPlain (255,255,255)
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
      let grey = TextOptions TSPlain (120, 120, 120)
          header = renderStyle HTML grey "# All-American Regex"
          slash = renderStyle HTML grey "/"
      in
      unlines
        [ "<style>body { background-color: black; } #regex { font-family: monospace; font-size: min(3.5vw, 5vh); }</style>\n"
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
  renderStyle mode whiteStyle str
renderOne opts@RenderOptions {..} (Prefix prefix tries) =
  renderStyle mode whiteStyle prefix ++ renderMany opts tries

joinBookends :: [(String, Char, String)] -> String
joinBookends = concatMap $ \(pre, c, post) -> pre ++ [c] ++ post

renderMany :: RenderOptions -> [Trie] -> [(String, Char, String)]
renderMany opts@RenderOptions {..} tries =
  concat
    [ renderStyle mode blueStyle "("
    , intercalate (renderStyle mode redStyle "|") $
        map (renderOne opts) tries
    , renderStyle mode blueStyle ")"
    ]

renderStyle :: RenderMode -> TextOptions -> String -> [(String, Char, String)]
renderStyle mode color str =
  let (pre, post) = rgbBookends mode color
  in
  map (pre,,post) str

rgbBookends :: RenderMode -> TextOptions -> (String, String)
rgbBookends ANSI (TextOptions style (r, g, b)) =
  ( concat
      [ "\ESC[38;2;"
      , intercalate ";" $ map show [r, g, b]
      , "m"
      ] ++ case style of
             TSBold -> "\ESC[1m"
             TSItalic -> "\ESC[3m"
             _ -> ""
  , "\ESC[0m"
  )
rgbBookends HTML (TextOptions style (r, g, b)) =
  ( concat
      [ "<span style=\"color: rgb("
      , intercalate "," $ map show [r, g, b]
      , ");"
      , case style of
          TSBold -> " font-weight: bold;"
          TSItalic -> " font-style: italic;"
          _ -> ""
      , "\">"
      ]
  , "</span>"
  )
rgbBookends Plain _ = ("", "")
