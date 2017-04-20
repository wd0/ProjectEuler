prob1 = sum $ filter p [1..999]
  where p = \x -> x `mod` 3 == 0 ||  x `mod` 5 == 0
  
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

fibs = map fib [1..]

prob2 = sum . filter even $ takeWhile (<4000000) fibs

prime n = not $ any divideEvenly uptoRoot
  where divideEvenly = \x -> n `mod` x == 0
        uptoRoot = [2..n] 
        
factors n = filter divideEvenly [1..n]
  where divideEvenly = \x -> n `mod` x == 0
