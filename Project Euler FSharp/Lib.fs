﻿module Lib

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

let rec factorise n =
    if n = 1 then [] else
    let a = [2 .. n] |> List.find (fun x -> n % x = 0)
    a :: factorise (n / a)
    