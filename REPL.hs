module REPL where

import Expr
import Parsing
import System.Exit

data State = State { vars :: [(Name, Int)],
                     numCalcs :: Int,
                     history :: [Command] }

initState :: State
initState = State [] 0 []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name val variables = dropVar name variables ++ [(name, val)]

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name variables = filter (\x -> fst x /= name) variables

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory state command = state { numCalcs = num, history = updatedHistory } where num = numCalcs state + 1
                                                                                    updatedHistory = history state ++ [command]

-- Get a command from the command history
getHistory :: Int -> [Command] -> Command
getHistory i commands = (commands!!(i))

process :: State -> Command -> IO ()
process st (Set var e)
     = do let st' = addHistory (st { vars = updateVars var (confirmInt (eval (vars st) (e))) (vars st) }) (Set var e)
          repl st'
process st (Eval e)
     = do let st' = addHistory (st { vars = updateVars "it" result (vars st) }) (Eval e)
          do putStrLn (show result) -- Print the result of evaluation
          repl st'
          where result = confirmInt (eval (vars st) (e))
process st (Print e)
     = do let st' = st
          do putStrLn ("OK\n" ++ (show result) ++ "\n")
          repl st'
          where result = (getHistory e (history st))

process st (Quit e)
     = do let st' = st
          exitSuccess where e == "quit"

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (numCalcs st) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st
