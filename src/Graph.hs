module Graph where
import Set(Set)
import qualified Set as Set
import Data.List (sort)

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
createUnionRelation graph1 graph2 = Set.union (relation graph1) (relation graph2)

createConnectDomain :: Set a -> Set a -> Set a
createConnectDomain = createUnionDomain

createConnectRelation :: Relation a -> Relation a -> Set (a, a)
createConnectRelation graph1 graph2 = 
  let pair1 = createPairList (Set.toList(domain graph1)) (Set.toList(domain graph2)) in
  -- let pair2 = createPairList (Set.toList(domain graph2)) (Set.toList(domain graph1)) in
  let new_relations = Set.fromList pair1 in 
  let old_relations = Set.union (relation graph1) (relation graph2) in
  Set.union new_relations old_relations

createPairs :: a -> [a] ->[(a, a)]
createPairs e = map (\ a -> (a, e))

createPairList :: [a] -> [a] -> [(a,a)]
createPairList list = concatMap (\ a -> createPairs a list)

instance Graph Relation where
  empty = Relation {domain = Set.empty, relation = Set.empty} 
  vertex a = Relation {domain = Set.singleton a, relation = Set.empty} 
  union graph1 graph2 = 
    Relation {
      domain = Set.union (domain graph1) (domain graph2), 
      relation = Set.union (relation graph1) (relation graph2)
    } 
  connect graph1 graph2 = 
    Relation {
      domain = Set.union (domain graph1) (domain graph2),
      relation = createConnectRelation graph1 graph2
    } 
                
instance (Ord a, Num a) => Num (Relation a) where
  fromInteger     = vertex . fromInteger
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

instance Ord a => Eq (Basic a) where
    graph1 == graph2 =
        let domain1 = domain (fromBasic graph1) in
        let domain2 = domain (fromBasic graph2) in
        let relations1 = relation (fromBasic graph1) in
        let relations2 = relation (fromBasic graph2) in
        domain1 == domain2 && relations1 == relations2

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

fromBasic Empty = empty 
fromBasic (Vertex a) = vertex a
fromBasic (Union g1 g2) = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

getAllVertex :: (Ord a, Show a) => Basic a -> [a]
getAllVertex graph = Set.toAscList (domain (fromBasic graph))

convertPairToList :: [(a,a)] -> [a]
convertPairToList = concatMap (\(x,y) -> [x,y])

getConnectedVertex :: (Ord a, Show a) => Basic a -> [a]
getConnectedVertex graph = Set.deleteDuplicates (sort (convertPairToList (Set.toAscList(relation (fromBasic graph)))))

getSingleVertex :: (Ord a, Show a) => Basic a -> [a]
getSingleVertex graph = 
  let connectedVertex = getConnectedVertex graph in
  let allVertex = getAllVertex graph in
  filter (`notElem` connectedVertex) allVertex

getAllRelations :: (Ord a, Show a) => Basic a -> [(a,a)]
getAllRelations graph = Set.toAscList (relation (fromBasic graph))

instance (Ord a, Show a) => Show (Basic a) where 
  show graph = 
    let edges = getAllRelations graph in
    let verticles = getSingleVertex graph in
    "edges " ++ show edges ++ " + verticles " ++ show verticles

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17


createEdgesString :: (Ord a, Show a) => [(a, a)] -> [Char] 
createEdgesString list = reverse (createEdgesString' list [])

insertCharAtBeggining :: [Char] -> [Char] -> [Char]
insertCharAtBeggining list acc = foldl (flip (:)) acc list

createEdgesString' :: (Ord a, Show a) => [(a, a)] -> [Char] -> [Char] 
createEdgesString' [] acc = acc
createEdgesString' ((a, b):t) acc = 
  let relation = show a ++ "->" ++ show b++ ";" in
  let new_acc = insertCharAtBeggining relation acc in
  createEdgesString' t new_acc


createVerticlesString :: (Ord a, Show a) => [a] -> [Char] 
createVerticlesString list = reverse (createVerticlesString' list [])

createVerticlesString' :: (Ord a, Show a) => [a] -> [Char] -> [Char] 
createVerticlesString' [] acc = acc
createVerticlesString' (h:t) acc = 
  let verticle = show h ++ ";" in
  let new_acc = insertCharAtBeggining verticle acc in
    createVerticlesString' t new_acc


todot :: (Ord a, Show a) => Basic a -> String
todot graph = 
  let edges = getAllRelations graph in
  let verticles = getSingleVertex graph in
  let edges_string = createEdgesString edges in
  let verticles_string = createVerticlesString verticles in
  "digraph {" ++ edges_string ++ verticles_string ++ "}"

instance Functor Basic where 
  fmap f Empty = Empty
  fmap f (Vertex a) = Vertex (f a)
  fmap f (Union g1 g2) = Union (fmap f g1) (fmap f g2)
  fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2) 

-- -- | Merge vertices
-- -- >>> mergeV 3 4 34 example34
-- -- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV v w destination (Vertex a) = if v == a || w == a then Vertex destination else Vertex a
mergeV v w destination (Connect g1 g2) = Connect (mergeV v w destination g1) (mergeV v w destination g2)
mergeV v w destination (Union g1 g2) = Union (mergeV v w destination g1) (mergeV v w destination g2)
mergeV _ _ _ Empty = Empty

-- instance Applicative Basic where


-- instance Monad Basic where
--          Empty >>= _ = Empty
--          Vertex v >>= f = f v
--          Union a b >>= f = Union (a >>= f) (b >>= f)
--          Connect a b >>= f = Connect (a >>= f) (b >>= f)
--          return v = Vertex v

-- -- | Split Vertex
-- -- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV v des1 des2 Empty = Empty
-- splitV v des1 des2 (Vertex a) = if a == v then Union (Vertex des1) (Vertex des2) else Vertex a

