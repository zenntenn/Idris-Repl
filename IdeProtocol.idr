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
data ReplStatus = Disconnected | Error | Ready | Waiting;

public export
ConnectTypes : ReplStatus;
ConnectTypes = Ready;
ConnectTypes = Error;

public export
SendTypes : ReplStatus;
SendTypes = Waiting;
SendTypes = Error;

public export
ReceiveTypes : ReplStatus;
ReceiveTypes = Ready;
ReceiveTypes = Waiting;
ReceiveTypes = Error;


public export
interface Repl (m : Type -> Type) where
{
  Status : ReplStatus -> Type;
  initialize : ST m Var [add (Status Disconnected)];
  connect : (repl : Var) -> ST m () [repl ::: Status Disconnected :-> Status ConnectTypes]; 
  disconnect : (repl : Var) -> ST m () [repl ::: Status ConnectTypes :-> Status Disconnected];
  send : (repl : Var) -> (message : String) -> ST m () [repl ::: Status Ready :-> Status SendTypes];
  -- Note, in this case String might either be a proper result, or an error message
  receive : (repl : Var) -> ST m String [repl ::: Status SendTypes :-> Status ReceiveTypes];
}


-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
 
