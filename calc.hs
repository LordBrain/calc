module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Control.Monad

import Lexcalc
import Parcalc
import Skelcalc
import Printcalc
import Abscalc




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s 

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree



showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> getContents >>= run 2 pExp
            "-s":fs -> mapM_ (runFile 0 pExp) fs
            fs -> mapM_ (runFile 2 pExp) fs





