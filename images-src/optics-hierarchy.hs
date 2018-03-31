import Control.Monad (when)
import Data.Foldable (for_)
import Text.Printf

nodes :: [(String, Float, Float)]
nodes =
    [ n "Equality" 1.5 0
    , n "Iso" 1.5 1
    , n "Lens" 0.75 2
    , n "Prism" 2.25 2
    , n "PrimGetter" 0 2
    , n "Getter" 0 3
    , n "PrimReview" 3 2
    , n "Review" 3 3
    , n "Traversal1" 0.75 3
    , n "AffineTraversal" 1.5 3
    , n "Fold1" 0 4
    , n "AffineFold" 0.75 4
    , n "Traversal" 1.5 4
    , n "Fold" 0.75 5
    , n "Setter" 1.5 5
    ] 
 where
    n = (,,)

findNode :: String -> Int
findNode name = go 0 nodes
  where
    go _ [] = error name
    go n ((name',_,_):ns) | name == name' = n
                          | otherwise     = go (n + 1) ns

edges :: [(String, String, [String])]
edges =
    [ {-  0 -} e "Equality" "Iso" ["black"]
    , {-  1 -} e "Iso" "Lens" ["strong"]
    , {-  2 -} e "Iso" "Prism" ["choice"]
    , {-  3 -} e "Lens" "Getter" ["strong"]
    , {-  4 -} e "PrimGetter" "Getter" ["getter"]
    , {-  5 -} e "Lens" "Traversal1" ["strong", "wander"]
    , {-  6 -} e "Lens" "AffineTraversal" ["strong"]
    , {-  7 -} e "Traversal" "Setter" ["strong", "wander", "choice", "carto"]
    , {-  8 -} e "Traversal" "Fold" ["strong", "wander", "choice"]
    , {-  9 -} e "Traversal1" "Fold1" ["strong", "wander"]
    , {- 10 -} e "Traversal1" "Traversal" ["strong", "wander"]
    , {- 11 -} e "Getter" "Fold1" ["getter", "strong"]
    , {- 12 -} e "AffineTraversal" "AffineFold" ["strong", "choice"]
    , {- 13 -} e "AffineFold" "Fold" ["getter", "strong", "choice"]
    , {- 14 -} e "Fold1" "Fold" ["getter", "strong", "wander"]
    , {- 15 -} e "Getter" "AffineFold" ["getter", "strong"]
    , {- 16 -} e "Prism" "AffineTraversal" ["choice"]
    , {- 17 -} e "AffineTraversal" "Traversal" ["strong", "choice"]
    , {- 18 -} e "Prism" "Review" ["choice"]
    , {- 19 -} e "PrimReview" "Review" ["review"]
    ]
  where
    e = (,,)

main :: IO ()
main = do
    readFile "header.mp" >>= putStr
    for_ (zip [0 :: Int ..] nodes) $ \(i, (name, x, y)) -> do
        printf "z%d=(%.2fdimXX,%.2fdimYY);\n" i x y
        printf "q%d=thelabel(btex $\\mathit{%s}$ etex, z%d);\n" i name i
        printf "draw q%d;\n\n" i

    for_ (zip [0 :: Int ..] edges) $ \(k, (a,b,cs)) -> do
        let i = findNode a
            j = findNode b
        printf "p%d=clippath(z%d..z%d, q%d, q%d);\n" k i j i j
        when (k == 15) $ do
            printf "h1 = p9 intersectionpoint p15;\n"
            printf "w1 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h1;\n"
        when (k == 12) $ do
            printf "h2 = p10 intersectionpoint p12;\n"
            printf "w2 = fullcircle xscaled (1.2*1.5mm) yscaled 1.2mm shifted h2;\n"
        printf "u%d=unitvector(direction 0 of p%d);\n" k k
        for_ (zip (offsets cs) cs) $ \((o, a), c) -> do
            let p = printf "(p%d shifted (%s*u%d rotated 90))" k o k :: String
            case a of
                R -> printf "filldraw rightArrowhead(%s) withcolor %sColor;\n" p c
                L -> printf "filldraw leftArrowhead(%s) withcolor %sColor;\n" p c
                B -> printf "filldraw bothArrowhead(%s) withcolor %sColor;\n" p c
                N -> pure ()
            case k of
                15 -> do
                    -- printf "draw w1;\n"
                      printf "draw subpath (ypart(w1 intersectiontimes %s), length %s) of (%s) withcolor %sColor;\n" p p p c
                      printf "draw subpath (0, ypart(w1 intersectiontimes (subpath (0, ypart(p9 intersectiontimes %s)) of (%s)))) of (subpath (0, ypart(p9 intersectiontimes %s)) of (%s)) withcolor %sColor;\n" p p p p c
                12 -> do
                    -- printf "draw w1;\n"
                      printf "draw subpath (ypart(w2 intersectiontimes %s), length %s) of (%s) withcolor %sColor;\n" p p p c
                      printf "draw subpath (0, ypart(w2 intersectiontimes (subpath (0, ypart(p10 intersectiontimes %s)) of (%s)))) of (subpath (0, ypart(p10 intersectiontimes %s)) of (%s)) withcolor %sColor;\n" p p p p c
                _  -> printf "draw (%s) withcolor %sColor;\n" p c
        
    readFile "footer.mp" >>= putStr

data Arr = N | L | B | R;

offsets :: [a] -> [(String, Arr)]
offsets [_]       = [("0mm", B)]
offsets [_,_]     = [("0.25mm", L), ("(-0.25mm)", R)]
offsets [_,_,_]   = [("0.5mm", L), ("0", N), ("(-0.5mm)", R)]
offsets [_,_,_,_] = [("0.75mm", L), ("0.25mm", N), ("(-0.25mm)", N), ("(-0.75mm)", R)]
