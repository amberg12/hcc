module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr)

import HCC.Assembly (assemble)
import HCC.Lexer (lexer)
import HCC.Parser (parser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (path : _) -> compile path
    [] -> do
      hPutStrLn stderr "Usage: hcc <file.c>"
      exitFailure

compile :: FilePath -> IO ()
compile path = do
  source <- readFile path
  let Just toks = lexer source
  let Just ast = parser toks
  let outPath = replaceExtension path ".s"
  writeFile outPath (assemble ast)
  putStrLn ("Wrote " ++ outPath)
