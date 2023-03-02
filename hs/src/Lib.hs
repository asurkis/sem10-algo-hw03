module Lib (Tree, insert, delete, add, set, sum, reverse, toList) where

import Prelude hiding (length, reverse, sum)

data PendingChange = Add Int | Set Int
  deriving (Eq, Show)

data Node = Node
  { nx :: Int,
    nLength :: Int,
    nHeight :: Int,
    nSum :: Int, -- Сумма до применения изменений
    nPendRev :: Bool,
    nPendChange :: PendingChange,
    nLeft :: Maybe Node,
    nRight :: Maybe Node
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

-- O(log L)
insert :: Int -> Int -> Tree -> Tree
insert i x t =
  let (tl, tr) = splitIndex i t
   in merge tl . Just $ insertFirst x tr

-- O(log L)
delete :: Int -> Tree -> Tree
delete i t =
  let (tl, tr) = splitIndex i t
   in merge tl $ deleteFirst tr

-- O(log L)
add :: Int -> Int -> Int -> Tree -> Tree
add l r x = change l r (Add x)

-- O(log L)
set :: Int -> Int -> Int -> Tree -> Tree
set l r x = change l r (Set x)

-- O(log L)
sum :: Int -> Int -> Tree -> Int
sum l r t =
  let (tlm, _) = splitIndex (r + 1) t
      (_, tm) = splitIndex l tlm
   in sumFull tm

-- O(log L)
reverse :: Int -> Int -> Tree -> Tree
reverse l r t =
  let (tlm, tr) = splitIndex (r + 1) t
      (tl, tm) = splitIndex l tlm
   in merge (merge tl $ reverseFull tm) tr

-- O(log L)
change :: Int -> Int -> PendingChange -> Tree -> Tree
change l r pc t =
  let (tlm, tr) = splitIndex (r + 1) t
      (tl, tm) = splitIndex l tlm
   in merge (merge tl $ changeFull pc tm) tr

-- O(log L)
merge :: Tree -> Tree -> Tree
merge Nothing t = t
merge t Nothing = t
merge tl (Just nr) =
  let (x, tr) = extractFirst nr
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
insertFirst x (Just n') =
  let n = normalize n'
   in rebalance $
        node (nx n) (Just . insertFirst x $ nLeft n) (nRight n)

-- O(log L)
deleteFirst :: Tree -> Tree
deleteFirst Nothing = Nothing
deleteFirst (Just n) = snd $ extractFirst n

-- O(log L)
extractFirst :: Node -> (Int, Tree)
extractFirst n' =
  let n = normalize n'
   in case nLeft n of
        Nothing -> (nx n, nRight n)
        Just n'' -> (\t -> Just . rebalance $ node (nx n) t (nRight n)) <$> extractFirst n''

-- O(1)
reverseFull :: Tree -> Tree
reverseFull = fmap $ \n ->
  Node
    (nx n)
    (nLength n)
    (nHeight n)
    (nSum n)
    (not $ nPendRev n)
    (nPendChange n)
    (nLeft n)
    (nRight n)

-- O(1)
changeFull :: PendingChange -> Tree -> Tree
changeFull pc = fmap $ \n ->
  Node
    (nx n)
    (nLength n)
    (nHeight n)
    (nSum n)
    (nPendRev n)
    (pc <> nPendChange n)
    (nLeft n)
    (nRight n)

-- Шаг ребалансировки АВЛ-дерева.
-- Наиболее проклятая часть решения.
rebalance :: Node -> Node
rebalance n
  | height (nLeft n) > height (nRight n) + 1 = case nLeft n of
      Just nl' ->
        let nl = normalize nl'
         in if height (nRight nl) <= height (nRight n)
              then node (nx nl) (nLeft nl) (Just $ node (nx n) (nRight nl) (nRight n))
              else case nRight nl of
                Just nlr' ->
                  let nlr = normalize nlr'
                   in node
                        (nx nlr)
                        (Just $ node (nx nl) (nLeft nl) (nLeft nlr))
                        (Just $ node (nx n) (nRight nlr) (nRight n))
                Nothing -> error "Impossible"
      Nothing -> error "Impossible"
  | height (nLeft n) + 1 < height (nRight n) = case nRight n of
      Just nr' ->
        let nr = normalize nr'
         in if height (nLeft nr) <= height (nRight nr)
              then node (nx nr) (Just $ node (nx n) (nLeft n) (nLeft nr)) (nRight nr)
              else case nLeft nr of
                Just nrl' ->
                  let nrl = normalize nrl'
                   in node
                        (nx nrl)
                        (Just $ node (nx n) (nLeft n) (nLeft nrl))
                        (Just $ node (nx nr) (nRight nrl) (nRight nr))
                Nothing -> error "Impossible"
      Nothing -> error "Impossible"
  | otherwise = n

-- O(1)
normalize :: Node -> Node
normalize n
  | nPendRev n = node (nx n) reversedRight reversedLeft
  | otherwise = node (nx n) changedLeft changedRight
  where
    changedLeft = changeFull (nPendChange n) (nLeft n)
    changedRight = changeFull (nPendChange n) (nRight n)
    reversedLeft = reverseFull changedLeft
    reversedRight = reverseFull changedRight

node :: Int -> Tree -> Tree -> Node
node x left right =
  Node
    x
    (1 + length left + length right)
    (1 + max (height left) (height right))
    (x + sumFull left + sumFull right)
    False
    mempty
    left
    right

-- Инвариант: если спустились в вершину,
-- то эта сумма корректна.
sumFull :: Tree -> Int
sumFull Nothing = 0
sumFull (Just n) = case nPendChange n of
  Add x -> x * nLength n + nSum n
  Set x -> x * nLength n

length :: Tree -> Int
length = maybe 0 nLength

height :: Tree -> Int
height = maybe 0 nHeight

toList :: Tree -> [Int]
toList Nothing = []

linearize (Just n') =
  let n = normalize n'
   in linearize (nLeft n) ++ nx n : linearize (nRight n)
