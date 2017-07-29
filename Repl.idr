import Control.ST
import Control.ST.ImplicitCall
import IdeProtocol

%default total

data CommunicationWithIdris = Connected | Disconnected

data IdrisResult = OK | Error

interface Repl (m : Type -> Type) where
  Idris : CommunicationWithIdris -> Type
  connect : ST m Var [add (Idris Disconnected)]
  disconnect : (idris : Var) -> ST m () [remove idris (Idris Disconnected)]
  loadIdris : (idris : Var) -> ST m IdrisResult [idris ::: Idris Disconnected :->
                                                   (\res => Idris (case res of
                                                                        OK => Connected
                                                                        Error => Disconnected))]

partial getData : (ConsoleIO m, Repl m) => (st, connected : Var) -> ST m() [st ::: Idris {m} Disconnected, connected ::: State CommunicationWithIdris]
getData st connected = do OK <- loadIdris st | Error => do putStrLn "Cannot connect to Idris backend"
                                                           getData st connected
                          putStr "Idris> "
                          p <- getStr 
                          -- ?firstPassParseCommand st connected p
                          -- getData st connected
                          ?getData_rhs_2

implementation Repl IO where
  Idris x = State String
  connect = do connection <- ?connect_rhs_1
               pure connection
  disconnect idris = delete idris
  loadIdris idris = ?loadIdris_rhs_1 

{- partial main : IO ()
main = run (do fc <- new Disconnected
               st <- connect
               getData st fc
               disconnect st
               delete fc)

-}

-- data Fuel = Dry | More (Lazy Main.Fuel)

run : Fuel -> ST IO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure Nothing

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do run forever greet
          pure ()
