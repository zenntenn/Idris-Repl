

%default total

data Parsed = Error | Quit | Send

isQuit : String -> Bool
isQuit ":q" = True
isQuit _ = False

stringAfterCommand : (s : String) -> String
stringAfterCommand s = let l = length s in
  case isLTE 2 l of
    (Yes prf) => substr 2 (l - 2) s
    (No contra) => "" 
{- stringAfterCommand s = case isLTE (length s) 3 of
                            (Yes prf) => ""
                            (No contra) => substr 2 ((length s) - 2) s
                            -}
                        --    False => "";
                         --   True => substr 2 ((length s)- 2) s;

parseTrimmed : (s : String) -> (Parsed, String)
parseTrimmed s = case isQuit (substr 0 2 s) of
                      True => (Quit, (stringAfterCommand s))
                      False => ?parseTrimmed_rhs_3

parse : (s : String) -> (Parsed, String)
parse s = parseTrimmed (trim s)
