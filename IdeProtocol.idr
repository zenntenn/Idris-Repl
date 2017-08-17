module IdeProtocol;

import Control.ST;
import Control.ST.ImplicitCall;
import System.Concurrency.Channels;
import System;

%default total

-- This should be updated to be smarter
connectionString : String;
connectionString = "/usr/bin/idris";

public export
data ReplStatus = Disconnected | Connected | Waiting;

public export
data ConnectionStatus = Ready | Error; 

public export
connectionStateTransition : ConnectionStatus -> ReplStatus;
connectionStateTransition Ready = Connected;
connectionStateTransition Error = Disconnected;

public export
sendStateTransition : ConnectionStatus -> ReplStatus;
sendStateTransition Ready = Waiting;
sendStateTransition Error = Disconnected;

public export
pollStateTransition : ConnectionStatus -> Either ReplStatus;
pollStateTransition Ready = ?pollStateTransition_rhs_1
pollStateTransition Error = Disconnected


public export
interface Repl (m : Type -> Type) where
{
  Status : ReplStatus -> Type;
  initialize : ST m Var [add (Status Disconnected)];
  connect : (repl : Var) -> ST m ConnectionStatus [repl ::: Status Disconnected :-> (\res => Status (connectionStateTransition res))]; 
  disconnect : (repl : Var) -> ST m ConnectionStatus [repl ::: Status Connected :-> Status Disconnected];
  send : (repl : Var) -> (message : String) -> ST m ConnectionStatus [repl ::: Status Connected :-> (\res => Status (sendStateTransition res))];
-- I'm pretty sure this is the wrong way to do things.  Instead the return should be atomic, but I can't figure out how to stick this in a tuple or some other data structure
  receive : (repl : Var) -> ST m String [repl ::: Status Waiting];
  pollForResponse : (repl : Var) -> ST m ConnectionStatus [repl ::: Status Waiting :-> (\res => Status (pollStateTransition res))]; 
}

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
 
