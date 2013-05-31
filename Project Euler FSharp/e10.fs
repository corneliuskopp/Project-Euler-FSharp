module e10
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
    Lib.primefactors 100I

let all =
    printfn "Four: %O" four
    printfn "Five: %O" five