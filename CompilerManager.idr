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
  receivePartial : (repl : Var) -> ST m ReplValues [repl ::: Status Waiting :-> (\res => Status (receiveStateTranstion res))];
  receiveFinal : (messagesLeft : Nat) -> (repl : Var) -> ST m ReplValues [repl ::: Status Waiting :-> Status (Error | OK)];
}

public export
-- This is to set a limit on how many messages can be reported back before an error condition has occured, otherwise it could get stuck in an infinite loop if Idris is behaving badly
maxMessages : Nat
maxMessages = 6

unableToStartIdris : String
unableToStartIdris = "Unable to start Idris from location ";

cantContactMessage : String
cantContactMessage = "Unable to contact Idris process";

concatinateMaybeStrings : Maybe String -> Maybe String -> Maybe String
concatinateMaybeStrings Nothing Nothing = Nothing
concatinateMaybeStrings Nothing (Just x) = Just x
concatinateMaybeStrings (Just x) Nothing = Just x
concatinateMaybeStrings (Just x) (Just y) = Just (x ++ y)

combineMessages : Maybe String -> ReplValues -> STrans IO ReplValues [(repl ::: (State ReplValues))] (\result1 => [(repl ::: (State ReplValues))])
combineMessages previousMessage newTransmission = do pure (record {message = concatinateMaybeStrings previousMessage (message newTransmission)} newTransmission)

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
                           Just chan => do { lift (system "killall idris"); -- This is a bad idea for several reasons, but I'm not sure how else to stop the idris process
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
  receivePartial repl = do { currentState <- read repl;
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
  receiveFinal Z repl = receivePartial repl
  receiveFinal (S messagesLeft) repl = do { currentMessage <- receivePartial repl;
                                            (case result currentMessage of
                                                  Failed => pure currentMessage;
                                                  OK => pure currentMessage;
                                                  More => do { newMessage <- receiveFinal messagesLeft repl;
                                                               combineMessages (message currentMessage) newMessage;
                                                             }
                                            )
                                          }


}
