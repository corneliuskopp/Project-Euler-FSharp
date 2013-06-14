module e20

open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Core.ExtraTopLevelOperators
open Lib

let one =
    let input = e20Data.two

    let matrix = DenseMatrix.OfArray input

    let findProdHoriz (m:DenseMatrix) =
        let rec work x y = 
            if x >= m.RowCount || (y + 3) > m.ColumnCount then
                []
            else if (y + 3) = m.ColumnCount then
                work (x+1) 0
            else
                let prod = m.[x,y] * m.[x,y+1] * m.[x,y+2] * m.[x,y+3]
                prod :: work x (y + 1)

        work 0 0

    let findProdVert (m:DenseMatrix) =
        let rec work x y =
            if y >= m.ColumnCount || (x + 3) > m.RowCount then
                []
            else if (x + 3) = m.RowCount then
                work 0 (y + 1)
            else
                let prod = m.[x,y] * m.[x+1,y] * m.[x+2,y] * m.[x+3,y]
                prod :: work (x + 1) y

        work 0 0

    let findProdDiag1 (m:DenseMatrix) =
        let maxX = m.RowCount - 1
        let maxY = m.ColumnCount - 4
        
        let getprod x y =
            List.fold (fun acc ctr -> acc * m.[x-ctr,y+ctr]) 1.0 [0..3]

        let rec findRight col x y =
            if col = maxY then
                [m.[x,y]]
            else
                let prod = getprod x y
                if y = maxY then
                    prod :: findRight (col + 1) maxX (col + 1)
                else
                    prod :: findRight col (x - 1) (y + 1)

        let rec findDown row x y = 
            if x = m.RowCount then
                findRight 0 (x - 1) 1
            else
                let prod = getprod x y

                if x = 3 then
                    prod :: findDown (row + 1) (row + 1) 0
                else
                    prod :: findDown row (x - 1) (y + 1)

        if m.RowCount < 4 || m.ColumnCount < 4 then
            [0.0]
        else
            let result = findDown 3 3 0
            result

    let horiz = maxOrZeroDouble (findProdHoriz matrix)
    let vert = maxOrZeroDouble (findProdVert matrix)
    let diag1 = maxOrZeroDouble (findProdDiag1 matrix)

    List.max [horiz, vert, diag1]

let two = 
    // http://www.urch.com/forums/gre-math/15597-find-all-positive-divisors-number.html
    let nextTriangleOption = (fun (step, acc) -> 
        let value = acc + step + 1
        
        let factorCount = 
               Seq.groupBy (fun v -> v) (primeFactors value)
            |> Map.ofSeq
            |> Map.map (fun k v -> Seq.length v)
            |> Map.fold (fun acc k v -> acc * (v + 1)) 1
                    
        Some((value,factorCount), ((step + 1), value))
        )
    let triangleSeq = Seq.unfold nextTriangleOption (0, 0)
        
    let skipBoringValuesSeq = Seq.skipWhile (fun (v,c) -> c < 500) triangleSeq
    Seq.exactlyOne (Seq.take 1 skipBoringValuesSeq)
    
let three = 
    let input = e20Data.three
    let rowSize = input.Length
    let mat = DenseMatrix.Create(rowSize, input.Head.ToString().Length, (fun x y -> 0.0))
    
    storeBigIntListInMatrix mat input

    let unitVector = DenseMatrix.Create(rowSize, 1, fun x y -> 1.0)
    let sumVector = mat.TransposeThisAndMultiply unitVector
    
    let ary = (sumVector.Column 0).ToArray()
    let columSumAry = List.rev (List.ofArray ary)

    let (sum, rem) = columSumAry |> List.fold (fun (acc, carry) currentSum -> 
                                    let localSum = currentSum + carry
                                    let currentDigit = localSum % 10.0
                                    let currentRemainder = (localSum - currentDigit) / 10.0
                                    
                                    // Debug
                                    //printfn "Carry: %3.0f Sum: %10.0f Digit: %3.0f Rem: %3.0f" carry currentSum currentDigit currentRemainder

                                    (currentDigit.ToString() + acc, currentRemainder)                                    
                                    ) ("", 0.0)

    let theSum = rem.ToString() + sum
    //printfn "Result: %A" theSum

    let answer = theSum.ToCharArray() |> Seq.ofArray |> Seq.take 10 |> Seq.fold (fun acc cur -> acc + cur.ToString()) ""
    answer

let all = 
    //printfn "one: %A" one
    //printfn "two: %A" two
    printfn "three: %A" three