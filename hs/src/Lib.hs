{-# LANGUAGE TemplateHaskell #-}
module Lib {-(Tree, insert, delete, add, set, sum, reverse)-} where

import           Control.Lens hiding (set)
import           Prelude      hiding (length, reverse, sum)

data PendingChange = Add Int | Set Int
  deriving (Eq, Show)

data Node = Node
  { _nx          :: Int,
    _nLength     :: Int,
    _nHeight     :: Int,
    _nSum        :: Int,
    _nPendRev    :: Bool,
    _nPendChange :: PendingChange,
    _nLeft       :: Maybe Node,
    _nRight      :: Maybe Node
  }
  deriving (Eq, Show)
makeLenses ''Node

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
  | i >= n ^. nLength = (Just n, Nothing)
  | i <= length (n ^. nLeft) =
    let (l, r) = splitIndex i $ n ^. nLeft
     in (l, Just . rebalance $ node (n ^. nx) r (n ^. nRight))
  | otherwise =
    let (l, r) = splitIndex (i - length (n ^. nLeft) - 1) $ n ^. nRight
     in (Just . rebalance $ node (n ^. nx) (n ^. nLeft) l, r)
  where
    n = normalize n'

-- O(log L)
insertFirst :: Int -> Tree -> Node
insertFirst x Nothing = node x Nothing Nothing
insertFirst x (Just n') = let n = normalize n' in rebalance $
  node (n ^. nx) (Just . insertFirst x $ n ^. nLeft) (n ^. nRight)

-- O(log L)
deleteFirst :: Tree -> Tree
deleteFirst Nothing  = Nothing
deleteFirst (Just n) = snd $ extractFirst n

-- O(log L)
extractFirst :: Node -> (Int, Tree)
extractFirst n' = let n = normalize n' in case n ^. nLeft of
  Nothing -> (n ^. nx, n ^. nRight)
  Just n'' -> (\t -> Just . rebalance $ node (n ^. nx) t (n ^. nRight)) <$> extractFirst n''

-- O(1)
reverseFull :: Tree -> Tree
reverseFull = fmap $ \n -> n & over nPendRev not

-- O(1)
changeFull :: PendingChange -> Tree -> Tree
changeFull pc = fmap $ \n -> n & over nPendChange (pc <>)

rebalance :: Node -> Node
rebalance = id -- undefined

-- O(1)
normalize :: Node -> Node
normalize = normalizeRev . normalizeChange
  where
    normalizeRev n
      | n ^. nPendRev = node (n ^. nx) (reverseFull $ n ^. nRight) (reverseFull $ n ^. nLeft)
      | otherwise = n

    normalizeChange n =
      let pc = n ^. nPendChange
       in node (n ^. nx)
          (changeFull pc $ n ^. nLeft)
          (changeFull pc $ n ^. nRight)

node :: Int -> Tree -> Tree -> Node
node x left right =
  Node x
  (1 + length left + length right)
  (1 + max (height left) (height right))
  (x + sumFull left + sumFull right)
  False mempty left right

sumFull :: Tree -> Int
sumFull Nothing = 0
sumFull (Just n) = case n ^. nPendChange of
  Add x -> x * n ^. nLength + n ^. nSum
  Set x -> x * n ^. nLength

length :: Tree -> Int
length = maybe 0 (^. nLength)

height :: Tree -> Int
height = maybe 0 (^. nHeight)

linearize :: Tree -> [Int]
linearize Nothing = []
linearize (Just n) = linearize (n ^. nLeft) ++ (n ^. nx) : linearize (n ^. nRight)
