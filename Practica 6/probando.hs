
{-data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

instance (Show a) => Show (Arbol a) where
  show Vacio = "Vacio"
  show (Nodo valor izq der) = "(Nodo <" ++ show izq ++ "> " ++ show valor ++ " <" ++ show der ++ ">)"-}



data BinTree a = Nil | Nodo (BinTree a) a (BinTree a)

instance (Show a) => Show (BinTree a) where
  show Nil = "Vacio"
  show (Nodo hi raiz hd) = "< Hijo izquierdo : " ++ show hi ++ ",  Raiz : " ++ show raiz ++ ",  Hijo derecho : " ++ show hd ++ ">"





