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

// http://amirrajan.net/Blog/prime-factors-kata-in-fsharp
let primeFactors number = 
    let rec factorsOf number factor accumulator = 
        if number > 1 && number % factor = 0 then 
            factorsOf (number / factor) (factor) (List.append accumulator [factor])
        else if number > 1 then 
            factorsOf (number) (factor + 1) (accumulator)
        else accumulator
    factorsOf number 2 []

let maxOrZeroDouble l = if List.isEmpty l then 0.0 else List.max l