module CompilerManager;

import Control.ST;
import Control.ST.ImplicitCall;
import System.Concurrency.Channels;
import System

%default total

public export
data ReplStatus = Disconnected | Error | Ready | Waiting;

public export
data ConnectionResult = Failed | OK | More

public export
record ReplValues where
{
  constructor MkReplValues
  result : ConnectionResult
  channel : Maybe Channel
  message : Maybe String
}

public export
connectStateTransition : ReplValues -> ReplStatus;
connectStateTransition (MkReplValues Failed pid message) = Error;
connectStateTransition (MkReplValues OK pid message) = Ready;
connectStateTransition (MkReplValues More pid message) = Error;

public export
sendStateTransition : ReplValues -> ReplStatus;
sendStateTransition (MkReplValues Failed pid message) = Error;
sendStateTransition (MkReplValues OK pid message) = Waiting;
sendStateTransition (MkReplValues More pid message) = Waiting;

public export
receiveStateTranstion : ReplValues -> ReplStatus;
receiveStateTranstion (MkReplValues Failed pid message) = Error;
receiveStateTranstion (MkReplValues OK pid message) = Ready;
receiveStateTranstion (MkReplValues More pid message) = Waiting;

public export
interface Repl (m : Type -> Type) where
{
  Status : ReplStatus -> Type;
  initialize : ST m Var [add (Status Disconnected)];
  connect : (repl : Var) -> (idrisLocation : String) -> ST m ReplValues [repl ::: Status Disconnected :-> (\res => Status (connectStateTransition res))];      
  disconnect : (repl : Var) -> ST m ReplValues [repl ::: Status (Error | Ready) :-> Status Disconnected];
  send : (repl : Var) -> (message : String) -> ST m ReplValues [repl ::: Status Ready :-> (\res => Status (sendStateTransition res))];
  receive : (repl : Var) -> ST m ReplValues [repl ::: Status Waiting :-> (\res => Status (receiveStateTranstion res))];
}

unableToStartIdris : String
unableToStartIdris = "Unable to start Idris from location ";

cantContactMessage : String
cantContactMessage = "Unable to contact Idris process";

public export
Repl IO where
{
  Status x = State ReplValues;
  initialize = new (MkReplValues Failed Nothing Nothing);
  connect repl idrisLocation = do { handle <- lift (spawn (system idrisLocation >>= \x => pure ()));
                                    currentState <- read repl;
                                    case (handle) of
                                    {
                                      Nothing => do pure (record {message = Just (unableToStartIdris ++ idrisLocation), result = Failed} currentState); 
                                      Just pid => do { chan <- lift (connect pid)
                                                       case (chan) of
                                                       {
                                                         Nothing => do pure (record {message = Just (unableToStartIdris ++ idrisLocation), result = Failed} currentState);
                                                         Just _ => do pure (record {result = OK, channel = chan} currentState);
                                                       }
                                                     } 
                                    }
                                  } 
  disconnect repl = do { currentState <- read repl;
                         case ( channel currentState) of
                         {
                           Nothing => do pure currentState;
                           Just chan => do { lift (system "killall idris"); -- This is a bad idea for several reasons, but I'm not sure how else ot stop the idris process
                                              pure (record {channel = Nothing} currentState);
                                           } 
                         }
                       }
  send repl message = do { currentState <- read repl;
                           case (channel currentState) of
                           {
                             Nothing => do pure (record {message = Just (cantContactMessage), result = Failed} currentState);
                             Just chan => do { sent <- lift (unsafeSend chan message);
                                               case (sent) of
                                               {
                                                 False => do pure (record {message = Just (cantContactMessage), result = Failed} currentState);
                                                 True => do pure (record {result = OK} currentState);
                                               }
                                             }
                           }
                         } 
  receive repl = do { currentState <- read repl;
                      case (channel currentState) of
                      {
                        Nothing => do pure (record {message = Just (cantContactMessage), result = Failed} currentState);
                        Just chan => do { fromMessage <- lift (unsafeRecv String chan);
                                          case (fromMessage) of
                                          {
                                            Nothing => do pure (record {message = Just (cantContactMessage), result = Failed} currentState);
                                            Just mess => do pure (record {result = OK, message = fromMessage} currentState);
                                          }
                                        }
                      }
                    }
}

 
 
