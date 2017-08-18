module IdeProtocol;

import Control.ST;
import Control.ST.ImplicitCall;
import System.Concurrency.Channels;
import Network

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
  receive : (repl : Var) -> ST m String [repl ::: Status Waiting :-> Status ReceiveTypes];
  getErrorMessage : (repl : Var) -> ST m String [repl ::: Status Error];
}

public export
implementation (Network.Sockets IO) => Repl IO where
{   
  Status _ = ?Repl_rhs_1
  initialize = ?Repl_rhs_2
  connect repl = ?Repl_rhs_3
  disconnect repl = ?Repl_rhs_4
  send repl message = ?Repl_rhs_5
  receive repl = ?Repl_rhs_6
  getErrorMessage repl = ?Repl_rhs_7
}


-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
 
