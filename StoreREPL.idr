module StoreREPL

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

addToStore : DataStore -> String -> DataStore
addToStore (MkData s is) i = MkData (s + 1) (is ++ the (Vect 1 _) [i])

emptyStore : DataStore
emptyStore = MkData 0 []

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
  False => Nothing
  True => Just (Get (cast val))
parseCommand "size" _ = Just Size
parseCommand "search" q = Just (Search q)
parseCommand "quit" _ = Just Quit
parseCommand _ _       = Nothing

parse : (input : String) -> Maybe Command
parse input = case (Strings.span (/= ' ') input) of
  (cmd, args) => parseCommand cmd (trim args)

search : String -> DataStore -> List (Nat, String)
search q (MkData _ []) = []
search q (MkData (S s) (i::is)) =
  if (Strings.isInfixOf q i) then
    (s, i) :: search q (MkData s is)
  else
    search q (MkData s is)

showSearch : List (Nat, String) -> String
showSearch [] = ""
showSearch ((i, s)::is) = show i ++ ": " ++ s ++ "\n" ++ (showSearch is)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store@(MkData size items) input = case parse input of
  Nothing         => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show size ++ "\n", addToStore store item)
  Just Size => Just (show size ++ "\n", store)
  Just (Search q) => Just (showSearch (search q store), store)
  Just (Get pos)  => case (integerToFin pos size) of
    Just id => Just (Data.Vect.index id items ++ "\n", store)
    Nothing => Just ("Out of range\n", store)
  Just Quit       => Nothing

main : IO ()
main = replWith emptyStore "$>" processInput
