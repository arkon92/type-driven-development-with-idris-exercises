import Control.Monad.State

update : (stateType -> stateType) -> State stateType ()
update f = do current <- get
              put (f current)

increase : Nat -> State Nat ()
increase x = update (+x)

-- *Exercise12-1> runState (increase 5) 89
-- ((), 94) : ((), Nat)


data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

countEmpty : Tree a -> State Nat ()
countEmpty Empty = update (+1)
countEmpty (Node left val right) = do countEmpty left
                                      countEmpty right

-- *Exercise12-1> execState (countEmpty testTree) 0
-- 7 : Nat


countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (empty, nodes) <- get
                          put (empty + 1, nodes)
countEmptyNode (Node left val right) = do countEmptyNode left
                                          (empty, nodes) <- get
                                          put (empty, nodes + 1)
                                          countEmptyNode right

-- *Exercise12-1> execState (countEmptyNode testTree) (0, 0)
-- (7, 6) : (Nat, Nat)