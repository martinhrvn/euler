problem31 = [(pn2, pn1, p50, p20, p10, p5, p2, p1) | 
                pn2 <- [0,200], 
                pn1 <- [0,100..200],
                p50 <- [0,50..200],
                p20 <- [0,20..200],
                p10 <- [0,10..200],
                p5  <- [0,5..200],
                p2 <- [0,2..200],
                p1 <- [0..200],
                p1+p2+p5+p10+p20+p50+pn1+pn2 == 200]

--main = print $ combine 1200 coins


coins :: [Int]
coins = [200,100,50,20,10,5,2,1]

combine :: Int -> [Int] -> Int
combine 0 xs = 1
combine n [] = 0
combine n (x:xs)
  | n < x       = combine n xs
  | n >= x      = combine (n-x) (x:xs) + combine n xs
               
pieces :: [Int]
pieces = [1,2,5,10,20,50,100,200]

beautiful = foldl (\without p ->
                          let (poor,rich) = splitAt p without
                              with = poor ++ 
                                     zipWith (++) (map (map (p:)) with)
                                                  rich
                          in with
                     ) ([[]] : repeat [])

main = print . length $ beautiful pieces !! 200


