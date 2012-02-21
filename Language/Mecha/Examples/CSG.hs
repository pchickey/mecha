-- | A constructive solid geometry widget.
module Language.Mecha.Examples.CSG (csg) where

import Language.Mecha

-- | A CSG widget.
csg :: Solid
csg = unions
  [ move ( 0, 0, 0) $ color (0.2, 0.2, 1.0, 1) $ difference sphereCube cyl3
  , move (-4, 0, 0) $ color (0.8, 0.0, 0.0, 1) $ sphereCube
  , move (-8, 0, 0) $ color (0.8, 0.0, 0.0, 1) $ sphere'
  , move (-4, 4, 0) $ color (0.8, 0.0, 0.0, 1) $ cube'
  , move ( 0, 4, 0) $ color (0.0, 0.8, 0.0, 1) $ cyl3
  , move ( 0, 8, 0) $ color (0.0, 0.8, 0.0, 1) $ cyl
  , move (-4, 8, 0) $ color (0.0, 0.8, 0.0, 1) $ rotateX (pi/2) cyl
  , move ( 4, 8, 0) $ color (0.0, 0.8, 0.0, 1) $ rotateY (pi/2) cyl
  , move ( 0, 12, 0) $ color (0.0, 0.8, 0.0, 1) $ triprism
  , move ( 0, 16, 0) $ color (0.0, 0.8, 0.0, 1) $ cutcube
  ]

sphere' = sphere 2
cube' = cube $ 2 * 0.75
sphereCube = intersection sphere' cube'
cyl = moveZ (-1) $ cylinder 1 2
cyl3 = unions [cyl, rotateX (pi / 2) cyl, rotateY (pi / 2) cyl]

triprism = scaleAll (0.025) $ polyhedron pts tris
  where
  pts = [ (  0, -10, 60 )
        , (  0,  10, 60 )
        , (  0,  10,  0 )
        , (  0, -10,  0 )
        , ( 60, -10, 60 )
        , ( 60,  10, 60 ) ]
  tris = [ ( 0, 3, 2 ) , ( 0, 2, 1 ) , ( 3, 0, 4 ) , ( 1, 2, 5 )
         , ( 0, 5, 4 ) , ( 0, 1, 5 ) , ( 5, 2, 4 ) , ( 4, 2, 3 ) ]

cutcube = difference cube' $ move (0, 0, -0.5) triprism
