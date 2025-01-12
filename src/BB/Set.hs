module BB.Set
  ( Set,
    -- CONSTRUCTORS
    empty,
    singleton,
    -- PROPERTIES
    null,
    size,
    height,
    -- SET OPERATIONS
    member,
    insert,
    delete,
    at,
    splitLt,
    splitGt,
    -- SET x SET OPERATIONS
    union,
    difference,
    intersection,
    -- FOLDING
    toAscList,
    toDescList,
    -- MIN & MAX
    minimum,
    maximum,
  )
where

import Prelude hiding (maximum, minimum, null)

data Set a
  = Nil
  | Node (Set a) Int a (Set a)

-- BALANCE PARAMETERS

omega, alpha :: Int
omega = 3
alpha = 2

-- PROPERTIES

null :: Set a -> Bool
null Nil = True
null Node {} = False

size :: Set a -> Int
size Nil = 0
size (Node _ s _ _) = s

height :: Set a -> Int
height Nil = 0
height (Node l _ _ r) = max (height l) (height r) + 1

-- CONSTRUCTORS

empty :: Set a
empty = Nil

singleton :: a -> Set a
singleton k = Node Nil 1 k Nil

node :: Set a -> a -> Set a -> Set a
node l k r = Node l (size l + 1 + size r) k r

balance :: Set a -> a -> Set a -> Set a
balance l k r
  | size l + size r <= 1 = node l k r
  | size r > omega * size l = case r of
      (Node rl _ _ rr)
        | size rl < alpha * size rr -> singleL l k r
        | otherwise -> doubleL l k r
      Nil -> error "unreachable"
  | size l > omega * size r = case l of
      (Node ll _ _ lr)
        | size lr < alpha * size ll -> singleR l k r
        | otherwise -> doubleR l k r
      Nil -> error "unreachable"
  | otherwise = node l k r

join :: (Ord a) => Set a -> Set a -> Set a
join Nil r = r
join l Nil = l
join l@(Node ll ls lk lr) r@(Node rl rs rk rr)
  | ls > omega * rs = balance ll lk (join lr r)
  | rs > omega * ls = balance (join l rl) rk rr
  | otherwise =
      if ls > rs
        then let (k, l') = extractMax l in balance l' k r
        else let (k, r') = extractMin r in balance l k r'

join3 :: (Ord a) => Set a -> a -> Set a -> Set a
join3 Nil k r = insertMin k r
join3 l k Nil = insertMax k l
join3 l@(Node ll ls lk lr) k r@(Node rl rs rk rr)
  | ls > omega * rs = balance ll lk (join3 lr k r)
  | rs > omega * ls = balance (join3 l k rl) rk rr
  | otherwise = node l k r

insertMin :: a -> Set a -> Set a
insertMin v Nil = node Nil v Nil
insertMin v (Node l _ k r) = balance (insertMin v l) k r

insertMax :: t -> Set t -> Set t
insertMax v Nil = node Nil v Nil
insertMax v (Node l _ k r) = balance l k (insertMax v r)

-- ROTATIONS

singleL :: Set a -> a -> Set a -> Set a
singleL l k (Node rl _ rk rr) = node (node l k rl) rk rr
singleL _ _ Nil = error "right subtree is empty"

singleR :: Set a -> a -> Set a -> Set a
singleR (Node ll _ lk lr) k r = node ll lk (node lr k r)
singleR Nil _ _ = error "left subtree is empty"

doubleL :: Set a -> a -> Set a -> Set a
doubleL l k (Node (Node rll _ rlk rlr) _ rk rr) = node (node l k rll) rlk (node rlr rk rr)
doubleL _ _ Nil = error "right subtree is empty"
doubleL _ _ (Node Nil _ _ _) = error "left subtree of right subtree is empty"

doubleR :: Set a -> a -> Set a -> Set a
doubleR (Node ll _ lk (Node lrl _ lrk lrr)) k r = node (node ll lk lrl) lrk (node lrr k r)
doubleR Nil _ _ = error "left subtree is empty"
doubleR (Node _ _ _ Nil) _ _ = error "right subtree of left subtree is empty"

-- SET OPERATIONS

member :: (Ord a) => a -> Set a -> Bool
member _ Nil = False
member v (Node l _ k r) =
  case v `compare` k of
    LT -> member v l
    EQ -> True
    GT -> member v r

insert :: (Ord a) => a -> Set a -> Set a
insert v Nil = node Nil v Nil
insert v (Node l _ k r) =
  case v `compare` k of
    LT -> balance (insert v l) k r
    EQ -> node l v r
    GT -> balance l k (insert v r)

delete :: (Ord a) => a -> Set a -> Set a
delete _ Nil = Nil
delete v (Node l _ k r) =
  case v `compare` k of
    LT -> balance (delete v l) k r
    EQ -> glue l r
    GT -> balance l k (delete v r)

glue :: Set a -> Set a -> Set a
glue Nil r = r
glue l Nil = l
glue l r =
  if size l > size r
    then let (k_, l_) = extractMax l in node l_ k_ r
    else let (k_, r_) = extractMin r in node l k_ r_

at :: Int -> Set a -> Maybe a
at _ Nil = Nothing
at idx (Node l _ k r) =
  let sl = size l
   in case idx `compare` sl of
        LT -> at idx l
        EQ -> Just k
        GT -> at (idx - sl - 1) r

splitLt :: (Ord a) => a -> Set a -> Set a
splitLt _ Nil = Nil
splitLt v (Node l _ k r) =
  case v `compare` k of
    LT -> splitLt v l
    EQ -> l
    GT -> join3 l k (splitLt v r)

splitGt :: (Ord a) => a -> Set a -> Set a
splitGt _ Nil = Nil
splitGt v (Node l _ k r) =
  case v `compare` k of
    LT -> join3 (splitGt v l) k r
    EQ -> r
    GT -> splitGt v r

-- SET x SET OPERATIONS

union :: (Ord a) => Set a -> Set a -> Set a
union Nil r = r
union l Nil = l
union l (Node rl _ rk rr) =
  let l' = splitLt rk l
      r' = splitGt rk l
   in join3 (l' `union` rl) rk (r' `union` rr)

difference :: (Ord a) => Set a -> Set a -> Set a
difference Nil _ = Nil
difference l Nil = l
difference l (Node rl _ rk rr) =
  let l' = splitLt rk l
      r' = splitGt rk l
   in join (l' `difference` rl) (r' `difference` rr)

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection Nil _ = Nil
intersection _ Nil = Nil
intersection l (Node rl _ rk rr) =
  let l' = splitLt rk l
      r' = splitGt rk l
   in if member rk l
        then join3 (l' `intersection` rl) rk (r' `intersection` rr)
        else join (l' `intersection` rl) (r' `intersection` rr)

-- FOLDING

instance Foldable Set where
  foldMap _ Nil = mempty
  foldMap f (Node l _ k r) = foldMap f l <> f k <> foldMap f r

toAscList :: Set a -> [a]
toAscList Nil = []
toAscList (Node l _ k r) = toDescList l ++ [k] ++ toDescList r

toDescList :: Set a -> [a]
toDescList Nil = []
toDescList (Node l _ k r) = toDescList r ++ [k] ++ toDescList l

-- MIN & MAX

minimum :: Set a -> Maybe a
minimum Nil = Nothing
minimum (Node l _ k _) =
  case l of
    Nil -> Just k
    Node {} -> minimum l

maximum :: Set a -> Maybe a
maximum Nil = Nothing
maximum (Node _ _ k r) =
  case r of
    Nil -> Just k
    Node {} -> maximum r

extractMin :: Set a -> (a, Set a)
extractMin Nil = error "set is empty"
extractMin (Node l _ k r) =
  case l of
    Nil -> (k, r)
    Node {} ->
      let (x, l_) = extractMin l
       in (x, balance l_ k r)

extractMax :: Set a -> (a, Set a)
extractMax Nil = error "set is empty"
extractMax (Node l _ k r) =
  case r of
    Nil -> (k, l)
    Node {} ->
      let (x, r_) = extractMax r
       in (x, balance l k r_)
