import Control.ST
import Control.ST.ImplicitCall

%default total

data Commands = Quit | Invalid

data Access = Connected | Disconnected

data Transition = Connecting | Disconnecting

interface ReplStatus (m : Type -> Type) where
  Status : Access -> Type
  connect : ST m Var [add (Status Disconnected)]
  disconnect : (status : Var) -> ST m () [remove status (Status Disconnected)]
  eval : (status : Var) -> ST m Transition [status ::: Status Disconnected :->
                                                    (\res => Status (case res of
                                                                          Connecting => Connected
                                                                          Disconnecting => Disconnected))]
  
  executeCommand : (status : Var) -> (result : String) -> (command : Commands) -> ST m Transition [status ::: Status Connected :->
                                                                                       (\res => Status (case res of
                                                                                                Connecting => Connected
                                                                                                Disconnecting => Disconnected))]
  
  interpretExpression : (status : Var) -> (result : String) -> ST m Transition [status ::: Status Connected :->
                                                                                       (\res => Status (case res of
                                                                                                Connecting => Connected
                                                                                                Disconnecting => Disconnected))]
  logout : (status : Var) -> ST m () [status ::: Status Connected :-> Status Disconnected]

startsWithColon : (s : String) -> Bool
startsWithColon s = substr 0 1 (ltrim s) == ":"

parseTrimmedCommand : (s : String) -> Commands
parseTrimmedCommand ":q" = Quit
parseTrimmedCommand _ = Invalid

parseCommand : (s : String) -> Commands
parseCommand s = parseTrimmedCommand (trim s)

isValidCommand : (s : String) -> Bool
isValidCommand s = case startsWithColon s of
                        False => False
                        True => case parseCommand s of
                                      Invalid => False
                                      _ => True


                                      
-- interpretExpression : (result : String) -> IO ()


partial input : (ConsoleIO m, ReplStatus m) => (st : Var) -> ST m () [st ::: Status {m} Disconnected]
input st = do Connecting <- eval st | Disconnecting => do putStrLn "Bye bye"
              logout st 
              input st
{-
        input st = do putStr "Idris> "
                      result <- getLine
                      case isValidCommand result of
                           True => executeCommand result (parseCommand result)
                           False => interpretExpression result
                           -}
{-
input : () -> (st2_fn : () -> Resources) -> STrans IO () [] (\__bindx => [])
input = do putStr "Idris> "
           result <- getLine
           case isValidCommand result of
                True => executeCommand result (parseCommand result)
                False => interpretExpression result
                -}

implementation ReplStatus IO where
  Status x = State String
  connect = do initial <- new "test"
               pure initial
  disconnect status = delete status
  eval st = do putStr "Idris> "
               result <- getStr
               case isValidCommand result of
                    True => executeCommand st result (parseCommand result) -- result (parseCommand result) st
                    False => interpretExpression st result
  executeCommand status result Quit = pure Disconnecting   
  executeCommand status result Invalid = pure Connecting 
  interpretExpression status result = pure Connecting -- Dummy stub code
  logout st = pure ()

partial main : IO ()
main = run (do st <- connect
               input st 
               disconnect st)
