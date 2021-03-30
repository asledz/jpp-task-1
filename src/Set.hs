module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, insert, deleteDuplicates
              ) where
import Prelude hiding(null)
import Data.List (sort)
import Data.Semigroup
import Data.Monoid

data Set a
  = Empty
  | Singleton a
  | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member a (Singleton b) = a == b
member a (Union x y) = member a x || member a y

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList l = fromList' l Empty

fromList' :: [a] -> Set a -> Set a
fromList' [] set = set
fromList' (h : t) set = fromList' t (insert h set)

toList :: Set a -> [a]
toList set = toList' set []

toList' :: Set a -> [a] -> [a]
toList' Empty list = list
toList' (Singleton a) list = a : list
toList' (Union x y) list = toList' x (toList' y list)

toAscList :: Ord a => Set a -> [a]
toAscList set = reverse (deleteDuplicates (sort (toList set)))

deleteDuplicates :: Ord a => [a] -> [a]
deleteDuplicates (h : t) = deleteDuplicates' h t [h]
deleteDuplicates [] = []

deleteDuplicates' :: Ord a => a -> [a] -> [a] -> [a]
deleteDuplicates' a [] acc = acc
deleteDuplicates' a (h : t) acc =
  if h == a
    then deleteDuplicates' a t acc
    else deleteDuplicates' h t (h : acc)

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
-- union with an empty set
union set Empty = set
union Empty set = set
-- union with singleton
union (Singleton a) set = insert a set
union set (Singleton a) = insert a set
-- other cases
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert a Empty = Singleton a
insert a (Singleton b) = Union (Singleton a) (Singleton b)
insert a g = Union g (Singleton a)

instance Ord a => Eq (Set a) where
  -- Empty set
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  -- Singleton set
  Singleton a == Singleton b = a == b
  -- Union set
  set1 == set2 = toAscList set1 == toAscList set2

instance Semigroup (Set a) where
  s1 <> s2 = union s1 s2

instance Monoid (Set a) where
  mempty = empty
  mappend = (<>)
  mconcat = foldr mappend mempty

instance Show a => Show (Set a) where
  show Empty = "[]"
  show (Singleton a) = show a
  show (Union s1 s2) = (show s1) ++ "," ++ (show s2)

instance Functor Set where
  fmap f Empty = Empty
  fmap f (Singleton a) = Singleton (f a)
  fmap f (Union s1 s2) = Union (fmap f s1) (fmap f s2)