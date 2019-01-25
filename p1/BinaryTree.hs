data Bin a = Node a (Bin a) (Bin a) | Leaf
    deriving (Show)

insert :: Ord a => a -> Bin a -> Bin a
insert ele Leaf = Node ele Leaf Leaf
insert ele (Node n left right)
    | ele >= n = Node n left (insert ele right)
    | otherwise = Node n (insert ele left) right