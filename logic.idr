n : Bool -> Bool
n True = False
n False = True

and : Bool -> Bool -> Bool
and True x = x
and False _ = False

or : Bool -> Bool -> Bool
or True _ = True
or False x = x
