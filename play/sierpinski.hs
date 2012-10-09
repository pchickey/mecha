
import qualified Language.Mecha as M
import Language.Mecha (Vector)

main :: IO ()
main = do writeFile "sierpinski.scad" $ M.openSCAD $ M.scaleAll 10 $ 
            mechanical $ cosierpinski 4


class Mechanical a where
  mechanical :: a -> M.Solid

data Prim = Tetrahedron Double
          | FlipX Prim
          | Offset Vector Prim
          | Union Prim Prim
          | Difference Prim Prim

instance Mechanical Prim where
  mechanical (Tetrahedron side) = M.scaleAll side $ unitTetrahedron
  mechanical (FlipX prim) = M.rotateX pi $ mechanical prim
  mechanical (Offset v prim) = M.move v $ mechanical prim
  mechanical (Union a b) = M.union (mechanical a) (mechanical b)
  mechanical (Difference a b) = M.difference (mechanical a) (mechanical b)


cosierpinski :: Int -> Prim
cosierpinski n = Difference (head sierpinskis)  (sierpinskis !! n)

sierpinski :: Int -> Prim
sierpinski n = sierpinskis !! n

sierpinskis :: [Prim]
sierpinskis = iterate gasket (Tetrahedron 1)

union :: Prim -> Prim -> Prim
union a b = Union a b

unions :: [Prim] -> Prim
unions as = foldl1 union as

unitTetrahedron =
  M.polyhedron
    [(0, 0, 0),
     (1, 0, 0),
     (0.5, (sqrt 3) / 2, 0),
     (0.5, (sqrt 3) / 6, (sqrt 3)/2)]
    [(0,1,2), (1,0,3), (2,1,3), (2,3,0)]

inner :: Prim -> Prim
inner s = Offset (0, (sqrt 3) / 3, (sqrt 3) / 4) $ FlipX $ s

gasket :: Prim -> Prim
gasket (Tetrahedron i) = unions
  [ first  $ Tetrahedron i2
  , second $ Tetrahedron i2
  , third  $ Tetrahedron i2
  , fourth $ Tetrahedron i2
  ]
  where
  first  = Offset (0, 0, 0)
  second = Offset (i/2, 0, 0)
  third  = Offset (i/4, i * (sqrt 3) / 4, 0)
  fourth = Offset (i/4, i * (sqrt 3) / 12, i * (sqrt 3) / 4)
  i2 = i / 2 + 0.001
gasket (Offset v prim) = Offset v (gasket prim)
gasket (Union p1 p2) = Union (gasket p1) (gasket p2)
