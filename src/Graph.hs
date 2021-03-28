module Graph where
import Set(Set)
import qualified Set as Set
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

createUnionDomain :: Set a -> Set a -> Set a
createUnionDomain = Set.union

createUnionRelation :: Relation a -> Relation a -> Set (a, a)
createUnionRelation graph1 graph2 = undefined

createConnectDomain :: Set a -> Set a -> Set a
createConnectDomain = createUnionDomain

createConnectRelation :: Relation a -> Relation a -> Set (a, a)
createConnectRelation graph1 graph2 = 
  let pair1 = createPairList (Set.toList(domain graph1)) (Set.toList(domain graph2)) in
  let pair2 = createPairList (Set.toList(domain graph2)) (Set.toList(domain graph1)) in
  let new_relations = Set.fromList (concat [pair1, pair2]) in 
  let old_relations = Set.union (relation graph1) (relation graph2) in
  Set.union new_relations old_relations

createPairs :: a -> [a] ->[(a, a)]
createPairs e = map (\ a -> (a, e))

createPairList :: [a] -> [a] -> [(a,a)]
createPairList list list2 = concatMap (\ a -> createPairs a list) list2

instance Graph Relation where
  empty = Relation {domain = Set.empty, relation = Set.empty} 
  vertex a = Relation {domain = Set.singleton a, relation = Set.empty} 
  union graph1 graph2 = 
    Relation {
      domain = Set.union (domain graph1) (domain graph2), 
      relation = Set.union (relation graph1) (relation graph2)
    } 
  connect = undefined 
                
instance (Ord a, Num a) => Num (Relation a) where
  fromInteger     = vertex . fromInteger --Relation {domain = Set.singleton x, relation = Set.empty}
  (+)             = union
  (*)             = connect
  signum          = const empty 
  abs             = id 
  negate          = id

instance Graph Basic where
    empty = Empty 
    vertex a = Vertex a 
    union a b = Union a b 
    connect a b = Connect a b 

-- instance Ord a => Eq (Basic a) where

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
    mempty = Empty
    mappend = (<>)
    mconcat = foldr mappend mempty

fromBasic :: Graph g => Basic a -> g a
fromBasic = undefined

-- instance (Ord a, Show a) => Show (Basic a) where

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot = undefined

-- instance Functor Basic where

-- -- | Merge vertices
-- -- >>> mergeV 3 4 34 example34
-- -- edges [(1,2),(2,34),(34,5)] + vertices [17]

-- mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- mergeV = undefined

-- instance Applicative Basic where

-- instance Monad Basic where

-- -- | Split Vertex
-- -- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV = undefined

