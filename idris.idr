import Control.ST
import Control.ST.ImplicitCall
import CompilerManager
import IdeProtocol

%default total

defaultIdrisLocation : String
defaultIdrisLocation = "/usr/bin/idris" -- Should make this smarter

data Access = Connected | Disconnected

data Transition = Connecting | Disconnecting

interface ReplStatus (m : Type -> Type) where
  Status : Access -> Type
  connect : (compilerLocation : String) -> ST m Var [add (Status Disconnected)]
  disconnect : (status : Var) -> ST m () [remove status (Status Disconnected)]
  eval : (status : Var) -> ST m Transition [status ::: Status Disconnected :->
                                                    (\res => Status (case res of
                                                                          Connecting => Connected
                                                                          Disconnecting => Disconnected))]

  execute : (status : Var) -> (Parsed, String) -> ST m Transition [status ::: Status Connected :->
                                                                                       (\res => Status (case res of
                                                                                                Connecting => Connected
                                                                                                Disconnecting => Disconnected))]

{-  interpretExpression : (status : Var) -> (result : String) -> ST m Transition [status ::: Status Connected :->
                                                                                       (\res => Status (case res of
                                                                                                Connecting => Connected
                                                                                                Disconnecting => Disconnected))]
                                                                                                -}
  logout : (status : Var) -> ST m () [status ::: Status Connected :-> Status Disconnected]

startsWithColon : (s : String) -> Bool
startsWithColon s = substr 0 1 (ltrim s) == ":"
{-
parseTrimmedCommand : (s : String) -> (Commands, Maybe String)
parseTrimmedCommand ":q" = (Quit, Nothing)
parseTrimmedCommand s = (Invalid, Just s)

outputCommand : Commands -> String
outputCommand Quit = ":q"
outputCommand Invalid = "Invalid Command"

takesNoArguments : Commands -> Bool;
takesNoArguments Quit = True;
takesNoArguments _ = False;

parseCommand : (s : String) -> (Commands, Maybe String)
parseCommand s = parseTrimmedCommand (trim s)

isValidCommand : (s : String) -> Bool
isValidCommand s = case startsWithColon s of
                        False => False
                        True => case parseCommand s of
                                      (Invalid, _) => False
                                      _ => True
                                      -}
partial input : (ConsoleIO m, ReplStatus m) => (st : Var) -> ST m () [st ::: Status {m} Disconnected]
input st = do Connecting <- eval st | Disconnecting => do putStrLn "Bye bye"
              logout st
              input st

outputMessageAndContinue : (st : Var) -> String -> STrans IO Transition [(st ::: (State ReplValues))] (\result => [(st ::: (State ReplValues))])
outputMessageAndContinue st x = do { putStrLn (x)
                                     pure Connecting;
                                   }

maybeStringNothingEmpty : Maybe String -> String
maybeStringNothingEmpty Nothing = ""
maybeStringNothingEmpty (Just x) = x


implementation ReplStatus IO where
  Status x = State ReplValues
  connect compilerLocation = do st <- initialize
                                CompilerManager.connect st compilerLocation
                                pure st
  disconnect status = delete status
  eval st = do putStr "Idris> "
               result <- getStr
               execute st (parse result)

  execute st (Quit, "") = pure Disconnecting
  execute st (Quit, s) = outputMessageAndContinue st ":q takes no arguments"

  execute st (Empty, _) = outputMessageAndContinue st ""
  execute st (Send, s) = do { send st s
                              returned <- receiveFinal maxMessages st
                              case result returned of
                                    OK => outputMessageAndContinue st (maybeStringNothingEmpty (message returned))
                                    Failed => outputMessageAndContinue st ("Error: " ++ (maybeStringNothingEmpty (message returned)))
                                    More => outputMessageAndContinue st "Error, should never happen, see label LABEL1 in idris.idr"
                            }

  logout st = pure ()

using (ReplStatus IO, Repl IO)
  partial runRepl : ST IO () []
  runRepl = do st <- connect defaultIdrisLocation
               input st
               disconnect st

partial main : IO ()
main = run runRepl
