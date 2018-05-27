

%default total

public export
data Parsed = Quit | Send | Empty

isQuit : (s : String) -> Bool
isQuit s = isPrefixOf ":q" s

remaining : List String -> String
remaining [] = ""
remaining (x :: xs) = unwords xs

stringAfterCommand : (s : String) -> String
stringAfterCommand s = remaining (words s)

interpretHead : (s : String) -> String
interpretHead ":l" = ":load-file"
interpretHead s = ":interpret " ++ s

interpretList : List String -> String
interpretList [] = "" -- Technically this should never be reached, but it's here for the totality checker.  I don't know how to write this differently so it never comes up
interpretList (x :: xs) = "(" ++ interpretHead x ++ unwords xs ++ "1)"    -- I'm assuming that http://docs.idris-lang.org/en/latest/reference/ide-protocol.html has a mistake for :load-file here

interpret : (s : String) -> String
interpret s = interpretList (words s)

public export
parseTrimmed : (s : String) -> (Parsed, String)
parseTrimmed "" = (Empty, "")
parseTrimmed s = case isPrefixOf ":q" s of
                      True => (Quit, (stringAfterCommand s))
                      False => (Send, (interpret s))

public export
parse : (s : String) -> (Parsed, String)
parse s = parseTrimmed (trim s)
