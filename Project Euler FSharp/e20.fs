module e20

open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Core.ExtraTopLevelOperators
open Lib

let one =
    let input = array2D [[08.0; 2.0; 22.0; 97.0; 38.0; 15.0; 0.0; 40.0; 0.0; 75.0; 4.0; 5.0; 7.0; 78.0; 52.0; 12.0; 50.0; 77.0; 91.0; 08.0];
                    [49.0; 49.0; 99.0; 40.0; 17.0; 81.0; 18.0; 57.0; 60.0; 87.0; 17.0; 40.0; 98.0; 43.0; 69.0; 48.0; 04.0; 56.0; 62.0; 00.0];
                    [81.0; 49.0; 31.0; 73.0; 55.0; 79.0; 14.0; 29.0; 93.0; 71.0; 40.0; 67.0; 53.0; 88.0; 30.0; 03.0; 49.0; 13.0; 36.0; 65.0];
                    [52.0; 70.0; 95.0; 23.0; 04.0; 60.0; 11.0; 42.0; 69.0; 24.0; 68.0; 56.0; 01.0; 32.0; 56.0; 71.0; 37.0; 02.0; 36.0; 91.0];
                    [22.0; 31.0; 16.0; 71.0; 51.0; 67.0; 63.0; 89.0; 41.0; 92.0; 36.0; 54.0; 22.0; 40.0; 40.0; 28.0; 66.0; 33.0; 13.0; 80.0];
                    [24.0; 47.0; 32.0; 60.0; 99.0; 03.0; 45.0; 02.0; 44.0; 75.0; 33.0; 53.0; 78.0; 36.0; 84.0; 20.0; 35.0; 17.0; 12.0; 50.0];
                    [32.0; 98.0; 81.0; 28.0; 64.0; 23.0; 67.0; 10.0; 26.0; 38.0; 40.0; 67.0; 59.0; 54.0; 70.0; 66.0; 18.0; 38.0; 64.0; 70.0];
                    [67.0; 26.0; 20.0; 68.0; 02.0; 62.0; 12.0; 20.0; 95.0; 63.0; 94.0; 39.0; 63.0; 08.0; 40.0; 91.0; 66.0; 49.0; 94.0; 21.0];
                    [24.0; 55.0; 58.0; 05.0; 66.0; 73.0; 99.0; 26.0; 97.0; 17.0; 78.0; 78.0; 96.0; 83.0; 14.0; 88.0; 34.0; 89.0; 63.0; 72.0];
                    [21.0; 36.0; 23.0; 09.0; 75.0; 00.0; 76.0; 44.0; 20.0; 45.0; 35.0; 14.0; 00.0; 61.0; 33.0; 97.0; 34.0; 31.0; 33.0; 95.0];
                    [78.0; 17.0; 53.0; 28.0; 22.0; 75.0; 31.0; 67.0; 15.0; 94.0; 03.0; 80.0; 04.0; 62.0; 16.0; 14.0; 09.0; 53.0; 56.0; 92.0];
                    [16.0; 39.0; 05.0; 42.0; 96.0; 35.0; 31.0; 47.0; 55.0; 58.0; 88.0; 24.0; 00.0; 17.0; 54.0; 24.0; 36.0; 29.0; 85.0; 57.0];
                    [86.0; 56.0; 00.0; 48.0; 35.0; 71.0; 89.0; 07.0; 05.0; 44.0; 44.0; 37.0; 44.0; 60.0; 21.0; 58.0; 51.0; 54.0; 17.0; 58.0];
                    [19.0; 80.0; 81.0; 68.0; 05.0; 94.0; 47.0; 69.0; 28.0; 73.0; 92.0; 13.0; 86.0; 52.0; 17.0; 77.0; 04.0; 89.0; 55.0; 40.0];
                    [04.0; 52.0; 08.0; 83.0; 97.0; 35.0; 99.0; 16.0; 07.0; 97.0; 57.0; 32.0; 16.0; 26.0; 26.0; 79.0; 33.0; 27.0; 98.0; 66.0];
                    [88.0; 36.0; 68.0; 87.0; 57.0; 62.0; 20.0; 72.0; 03.0; 46.0; 33.0; 67.0; 46.0; 55.0; 12.0; 32.0; 63.0; 93.0; 53.0; 69.0];
                    [04.0; 42.0; 16.0; 73.0; 38.0; 25.0; 39.0; 11.0; 24.0; 94.0; 72.0; 18.0; 08.0; 46.0; 29.0; 32.0; 40.0; 62.0; 76.0; 36.0];
                    [20.0; 69.0; 36.0; 41.0; 72.0; 30.0; 23.0; 88.0; 34.0; 62.0; 99.0; 69.0; 82.0; 67.0; 59.0; 85.0; 74.0; 04.0; 36.0; 16.0];
                    [20.0; 73.0; 35.0; 29.0; 78.0; 31.0; 90.0; 01.0; 74.0; 31.0; 49.0; 71.0; 48.0; 86.0; 81.0; 16.0; 23.0; 57.0; 05.0; 54.0];
                    [01.0; 70.0; 54.0; 71.0; 83.0; 51.0; 54.0; 69.0; 16.0; 92.0; 33.0; 48.0; 61.0; 43.0; 52.0; 01.0; 89.0; 19.0; 67.0; 48.0]]

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
    
let all = 
    //printfn "one: %A" one
    printfn "two: %A" two