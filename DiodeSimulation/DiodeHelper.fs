module DiodeHelper

open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra

type DiodeData = {
    Conductance : ((int * int) * float) list
    ConductanceDerivative : (int * int) list
    Current : float list
    CurrentDiodeElement : int list
    Vd : (int * int)
}

let getNodeNumber (lst : (int*int) list) =
    let maxX = List.maxBy fst lst |> fst
    let maxY = List.maxBy snd lst |> snd

    if maxX > maxY 
    then maxX
    else maxY

let firstPart (lst : ((int * int)*float) list) = 
    lst
    |> List.map(fun x -> fst x)

let getMatrix (lst : ((int*int) * float) list ) = 

    let coordList = firstPart lst
    let nodeNumber = getNodeNumber coordList 
    let mat = Matrix.Create(nodeNumber,nodeNumber)

    (mat , lst)
    ||> List.fold (fun mat lst -> let index = fst lst
                                  let value = snd lst
                                  let firstindex = fst index
                                  let secondindex = snd index
                                  mat.[(firstindex-1),(secondindex-1)] <- value
                                  mat.[(secondindex-1),(firstindex-1)] <- value
                                  mat
                          )

let addOnVector (vec:DenseVector<float>)(lst : int list)(diodeC:float) = 
    (vec, lst)
    ||> List.fold(fun vecRes index -> let original = vecRes[index-1]
                                      let sum = original + diodeC
                                      vecRes[index-1] <- -sum
                                      vecRes)