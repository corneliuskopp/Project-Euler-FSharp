module Lib

let primes = 
    let rec nextPrime n p primes =
        if primes |> Map.containsKey n then
            nextPrime (n + p) p primes
        else
            primes.Add(n, p)

    let rec prime n primes =
        seq {
            if primes |> Map.containsKey n then
                let p = primes.Item n
                yield! prime (n + 1I) (nextPrime (n + p) p (primes.Remove n))
            else
                yield n
                yield! prime (n + 1I) (primes.Add(n * n, n))
        }

    prime 2I Map.empty

let primefactors x =
//    let handlePrime number prime = 
//            if number % prime = 0 then
//                let newNumber = number / prime
//                prime :: (handlePrime newNumber prime)
//            else
//                []
//
//
//    let checkValue number = handlePrime x 2
//    
//    checkValue x
    1
    