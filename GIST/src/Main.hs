module Main
where
main = do (putStrLn.show) "lol"

class Key c where
    consistent :: Entry -> Predicate -> Bool
    union :: [Entry] -> Predicate
    penalty :: Entry -> Entry -> Penalty
    pickSplit :: [Entry] -> [[Entry]]


class Entry e

class Predicate p

class Penalty pen

