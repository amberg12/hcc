module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr)

import HCC.AST (ast)
import HCC.Assembly (assemble)
import HCC.IR (emitIR)
import HCC.Lexer (lexer)

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

  case lexer source of
    Nothing -> do
      hPutStrLn stderr "Lexer error"
      exitFailure
    Just toks ->
      case ast toks of
        Nothing -> do
          hPutStrLn stderr "Parser error"
          exitFailure
        Just ast -> do
          let outPath = replaceExtension path ".s"
          writeFile outPath (assemble $ emitIR ast)
