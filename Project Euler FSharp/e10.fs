module e10
open Lib

let min = 900
    
let four =
    let result = ref 0

    let check v1 v2 = 
        let prod = v1 * v2
        let ps = prod.ToString()
        let len = ps.Length

        let mutable success = true

        let ceil = (len - (len % 2)) / 2
        let maxin = len - 1
        for i in [0..ceil] do
            let left = ps.Chars i
            let right = ps.Chars (maxin - i)
            if not (left.Equals(right)) then success <- false

        if success then 
            result := prod

            (* Debug output *)
            (*for i in [0..ceil] do
                let rind = maxin - i
                let left = ps.Chars i
                let right = ps.Chars rind
                printfn "(%d) %O <-> %O (%d)" i left right rind*)

        success

    let s1 = Seq.unfold (fun state ->
        if state < min then None else 
        let newval = state - 1
        Some (newval, newval)) 1000

    let s2 = Seq.unfold (fun state ->
        if state < min then None else 
        let newval = state - 1
        Some (newval, newval)) 1000

    let cont = ref true
        
    for v1 in Seq.takeWhile (fun e -> !cont) s1 do
        for v2 in Seq.takeWhile (fun e -> !cont) s2 do
            cont := not (check v1 v2)
    !result
    
let five =
    1

let six = 
    let theDiff n = 
        let sqSum = (List.sum [1I..n]) ** 2
        let sumSq = List.fold(fun acc elem -> acc + elem ** 2) 0I [1I..n]
        
        printfn "%A" sqSum
        printfn "%A" sumSq
                
        abs sqSum - sumSq
    theDiff 100I

let seven =
    let skip = Seq.skip 10000 primes
    Seq.take 1 skip

let eight = 
    let str = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    let strList = List.ofSeq str

    let toInt x = System.Convert.ToInt32(x.ToString())

    let rec fiverMult = fun x ->
        match x with
            |  a :: b :: c :: d :: e :: tail -> 
                let firstElem = (toInt a) * (toInt b) * (toInt c) * (toInt d) * (toInt e)
                let rest = b :: c :: d :: e :: tail
                firstElem :: fiverMult rest
            | _ -> []
    
    let foo = fiverMult strList
    List.max foo

let nine =
    let cutoff = 100I

    let generator = fun (u:bigint, v:bigint) ->         
        // taken from http://de.wikipedia.org/wiki/Pythagoreisches_Tripel
        let usq = u ** 2
        let vsq = v ** 2

        let x = usq - vsq
        let y = 2I * u * v
        let z = usq + vsq
        
        // Debugging
//        let theSum = x + y + z
//        printfn "Checking for (%A, %A): %A + %A + %A = %A" u v x y z theSum

        if (v > cutoff) then
            None
        elif (u > cutoff) then
            Some((x, y, z), (v + 2I, v + 1I))
        else
            Some((x, y, z), (u + 1I, v))
    
    let pythSeq = Seq.unfold generator (2I, 1I)
    let whileSeq = Seq.skipWhile (fun (x:bigint, y:bigint, z:bigint) -> not (x + y + z = 1000I)) pythSeq
    let (a, b, c) = Seq.exactlyOne (Seq.take 1 whileSeq)
    let prod = a * b * c
    printfn "Found result: %A * %A * %A = %A" a b c prod
    prod

let ten =
    // Find the sum of all the primes below two million.
    let start = System.DateTime.Now
    
    let primesUnder2Mil = Seq.takeWhile (fun x -> x < 2000000I) primes
    let folder = fun acc elem -> acc + elem

    let result = Seq.fold folder 0I primesUnder2Mil
    let theEnd = System.DateTime.Now
    printfn "End: %A - %A = %A"  start theEnd (theEnd - start)
    result

let all =
//    printfn "Four: %O" four
//    printfn "Five: %O" five
//    printfn "Six: %A" six
//    printfn "Seven: %A" seven
//    printfn "Eight: %A" eight
//    printfn "Nine: %A" nine
    printfn "Ten: %A" ten