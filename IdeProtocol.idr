module IdeProtocol

import Control.ST
import Control.ST.ImplicitCall
import System.Concurrency.Channels
import System

%default total

-- This should be updated to be smarter
connectionString : String
connectionString = "/usr/bin/idris"

public export
data ReplStatus = Disconnected | Connected

public export
data ConnectionStatus = Active | Error

public export
interface Repl (m : Type -> Type) where
  Status : ReplStatus -> Type
  initialize : ST m Var [add (Status Disconnected)]
  connect : (repl : Var) -> ST m ConnectionStatus [repl ::: Status Disconnected :-> 
                                                    (\res => Status (case res of
                                                                         Active => Connected
                                                                         Error => Disconnected))] 
  disconnect : (repl : Var) -> ST m ConnectionStatus [repl ::: Status Connected :-> Status Disconnected]
                                                          


-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
