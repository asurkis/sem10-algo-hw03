{-# LANGUAGE TemplateHaskell #-}
module Lib (Tree, Node (..), leaf, insert, delete, add, set, sum, reverse) where

import           Control.Lens hiding (set)
import           Prelude      hiding (length, reverse, sum)

data PendingChange = Add Int | Set Int

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
makeLenses ''Node

type Tree = Maybe Node

instance Semigroup PendingChange where
  Add x <> Add y = Add $ x + y
  Add x <> Set y = Set $ x + y
  Set x <> Set y = Set $ x + y
  Set x <> Add _ = Set x

instance Monoid PendingChange where
  mempty = Add 0

leaf :: Int -> Node
leaf x = Node x 1 1 x False mempty Nothing Nothing

insert :: Int -> Int -> Tree -> Tree
insert i x t = undefined

delete :: Int -> Tree -> Tree
delete i t = undefined

add :: Int -> Int -> Int -> Tree -> Tree
add l r x t = undefined

set :: Int -> Int -> Int -> Tree -> Tree
set l r x t = undefined

sum :: Int -> Int -> Tree -> Int
sum l r t = undefined

reverse :: Int -> Int -> Tree -> Tree
reverse l r t = undefined

change :: Int -> Int -> PendingChange -> Tree -> Tree
change = undefined

-- O(1)
reverseFull :: Tree -> Tree
reverseFull = fmap $ \n -> n & over nPendRev not

-- O(1)
changeFull :: PendingChange -> Tree -> Tree
changeFull pc = fmap $ \n -> n & over nPendChange (pc <>)

-- O(1)
normalize :: Tree -> Tree
normalize = fmap $ normalizeRev . normalizeChange
  where
    normalizeRev n
      | n ^. nPendRev = n
        & nLeft .~ (n ^. nRight)
        & nRight .~ (n ^. nLeft)
        & over nLeft reverseFull
        & over nRight reverseFull
        & nPendRev .~ False
      | otherwise = n

    normalizeChange n = n
      & over nLeft (changeFull $ n ^. nPendChange)
      & over nRight (changeFull $ n ^. nPendChange)
      & nPendChange .~ mempty

length :: Maybe Node -> Int
length = maybe 0 (^. nLength)

height :: Maybe Node -> Int
height = maybe 0 (^. nHeight)
