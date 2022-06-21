module LinearSimulation

open System
open System.IO
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
open Helper

//----------Linear Simulation------------

/// Gets the node number from the index list
let getNodeNumber (lst : (int*int) list) =
    let maxX = List.maxBy fst lst |> fst
    let maxY = List.maxBy snd lst |> snd

    if maxX > maxY 
    then maxX
    else maxY

/// Obtains the index list from the conductance list
let firstPart (lst : ((int * int)*float) list) = 
    lst
    |> List.map(fun x -> fst x)

/// ((int * int) * float) list to Matrix
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

/// Top-level function to simulate linear circuits
let LinearSimulation (data: LinearCircuitData) = 
    let conductance = getMatrix data.Conductance
    let current = data.Current

    let nodeNumber = data.Conductance
                     |> firstPart
                     |> getNodeNumber

    let currentVector = Vector.Create (nodeNumber)
    currentVector.[0] <- current

    conductance * currentVector