import Control.Applicative

fac n = product [1..n]

length . filter (>1000000) $ (\x y -> (fac x) `div` (fac y * fac (x-y))) <$> [1..100] <*> [1..100]
