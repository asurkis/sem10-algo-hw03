module Lib {-(Tree, insert, delete, add, set, sum, reverse)-} where

import           Prelude hiding (length, reverse, sum)

data PendingChange = Add Int | Set Int
  deriving (Eq, Show)

data Node = Node
  { nx          :: Int,
    nLength     :: Int,
    nHeight     :: Int,
    nSum        :: Int, -- Сумма до применения изменений
    nPendRev    :: Bool,
    nPendChange :: PendingChange,
    nLeft       :: Maybe Node,
    nRight      :: Maybe Node
  }
  deriving (Eq, Show)

type Tree = Maybe Node

instance Semigroup PendingChange where
  Add x <> Add y = Add $ x + y
  Add x <> Set y = Set $ x + y
  Set x <> Set y = Set $ x + y
  Set x <> Add _ = Set x

instance Monoid PendingChange where
  mempty = Add 0

insert :: Int -> Int -> Tree -> Tree
insert i x t = let (tl, tr) = splitIndex i t
                in merge tl . Just $ insertFirst x tr

delete :: Int -> Tree -> Tree
delete i t = let (tl, tr) = splitIndex i t
              in merge tl $ deleteFirst tr

add :: Int -> Int -> Int -> Tree -> Tree
add l r x = change l r (Add x)

set :: Int -> Int -> Int -> Tree -> Tree
set l r x = change l r (Set x)

sum :: Int -> Int -> Tree -> Int
sum l r t =
  let (tlm, _) = splitIndex (r + 1) t
      (_, tm) = splitIndex l tlm
   in sumFull tm

reverse :: Int -> Int -> Tree -> Tree
reverse l r t =
  let (tlm, tr) = splitIndex (r + 1) t
      (tl, tm) = splitIndex l tlm
   in merge (merge tl $ reverseFull tm) tr

change :: Int -> Int -> PendingChange -> Tree -> Tree
change l r pc t =
  let (tlm, tr) = splitIndex (r + 1) t
      (tl, tm) = splitIndex l tlm
   in merge (merge tl $ changeFull pc tm) tr

-- O(log L)
merge :: Tree -> Tree -> Tree
merge Nothing t = t
merge t Nothing = t
merge tl (Just nr) = let (x, tr) = extractFirst nr
                      in Just $ node x tl tr

-- O(log L)
splitIndex :: Int -> Tree -> (Tree, Tree)
splitIndex _ Nothing = (Nothing, Nothing)
splitIndex i (Just n')
  | i <= 0 = (Nothing, Just n)
  | i >= nLength n = (Just n, Nothing)
  | i <= length (nLeft n) =
    let (l, r) = splitIndex i $ nLeft n
     in (l, Just . rebalance $ node (nx n) r (nRight n))
  | otherwise =
    let (l, r) = splitIndex (i - length (nLeft n) - 1) $ nRight n
     in (Just . rebalance $ node (nx n) (nLeft n) l, r)
  where
    n = normalize n'

-- O(log L)
insertFirst :: Int -> Tree -> Node
insertFirst x Nothing = node x Nothing Nothing
insertFirst x (Just n') = let n = normalize n' in rebalance $
  node (nx n) (Just . insertFirst x $ nLeft n) (nRight n)

-- O(log L)
deleteFirst :: Tree -> Tree
deleteFirst Nothing  = Nothing
deleteFirst (Just n) = snd $ extractFirst n

-- O(log L)
extractFirst :: Node -> (Int, Tree)
extractFirst n' = let n = normalize n' in case nLeft n of
  Nothing -> (nx n, nRight n)
  Just n'' -> (\t -> Just . rebalance $ node (nx n) t (nRight n)) <$> extractFirst n''

-- O(1)
reverseFull :: Tree -> Tree
reverseFull = fmap $ \n -> Node (nx n) (nLength n) (nHeight n)
  (nSum n) (not $ nPendRev n) (nPendChange n) (nLeft n) (nRight n)

-- O(1)
changeFull :: PendingChange -> Tree -> Tree
changeFull pc = fmap $ \n -> Node (nx n) (nLength n) (nHeight n)
  (nSum n) (nPendRev n) (pc <> nPendChange n) (nLeft n) (nRight n)

rebalance :: Node -> Node
rebalance = id -- undefined

-- O(1)
normalize :: Node -> Node
normalize = normalizeRev . normalizeChange
  where
    normalizeRev n
      | nPendRev n = node (nx n) (reverseFull $ nRight n) (reverseFull $ nLeft n)
      | otherwise = n

    normalizeChange n =
      let pc = nPendChange n
       in node (nx n)
          (changeFull pc $ nLeft n)
          (changeFull pc $ nRight n)

node :: Int -> Tree -> Tree -> Node
node x left right =
  Node x
  (1 + length left + length right)
  (1 + max (height left) (height right))
  (x + sumFull left + sumFull right)
  False mempty left right

sumFull :: Tree -> Int
sumFull Nothing = 0
sumFull (Just n) = case nPendChange n of
  Add x -> x * nLength n + nSum n
  Set x -> x * nLength n

length :: Tree -> Int
length = maybe 0 nLength

height :: Tree -> Int
height = maybe 0 nHeight

linearize :: Tree -> [Int]
linearize Nothing  = []
linearize (Just n) = linearize (nLeft n) ++ nx n : linearize (nRight n)
