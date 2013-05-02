class Key c where
	consistent :: Entry -> Predicate -> Bool
	union :: [Entry] -> Predicate 
	penalty :: Entry -> Entry -> Penalty
	pickSplit :: [Entries] -> [[Entries]]
  
	
