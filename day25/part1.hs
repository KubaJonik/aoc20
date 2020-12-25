
main :: IO ()
main = readFile "input.txt" >>= print . solve . parse

solve :: (Int, Int) -> Int
solve (k1, k2) = loopFor (loopSize k2) k1

loopSize :: Int -> Int
loopSize n = fst . head . dropWhile (\(_,p) -> p /= n) $ ek
  where ek = [0..] `zip` iterate (loop 7) 1

loop :: Int -> Int -> Int
loop sn k = (sn*k) `mod` 20201227

loopFor :: Int -> Int -> Int
loopFor n sn = iter (loop sn) 1 n

iter :: (Int -> Int) -> Int -> Int -> Int
iter _ v 0 = v
iter f v n = (iter f $! f v) $! (n-1)

parse :: String -> (Int, Int)
parse s = let (n1:n2:_) = lines s
           in (read n1, read n2)
