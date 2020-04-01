{-# LANGUAGE QuasiQuotes #-}

module Eval where

import Control.Exception
import Data.Char (isSpace)
import Data.List
import Data.String.Interpolate (i)
import Language.Haskell.Interpreter hiding (eval)
import qualified Language.Haskell.Interpreter as L (eval)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

interpreter :: MonadInterpreter m => (m ())
interpreter = setImports ["Prelude"]

evaluate :: String -> IO ()
evaluate command
  | ":t" `isPrefixOf` command = do
    let commandProper = trim . drop 2 $ command
    result <- runInterpreter $ interpreter >> typeOf commandProper
    case result of
      Left  e -> putStrLn . displayException $ e
      Right v -> putStrLn [i|#{commandProper} :: #{v}|]
  | otherwise = do
    result <- runInterpreter $ interpreter >> L.eval command
    case result of
      Left  e -> putStrLn . displayException $ e
      Right v -> putStrLn [i|Prelude> #{command}|] >> putStrLn v
