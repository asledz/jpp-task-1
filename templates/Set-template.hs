module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a

null :: Set a -> Bool

member :: Eq a => a -> Set a -> Bool

singleton :: a -> Set a

fromList :: [a] -> Set a

toList :: Set a -> [a]

toAscList :: Ord a => Set a -> [a]

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a

insert :: a -> Set a -> Set a

instance Ord a => Eq (Set a) where

instance Semigroup (Set a) where

instance Monoid (Set a) where

instance Show a => Show (Set a) where

instance Functor Set where
