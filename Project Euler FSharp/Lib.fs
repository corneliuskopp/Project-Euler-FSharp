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

let maxOrZeroDouble l = if List.isEmpty l then 0.0 else List.max l