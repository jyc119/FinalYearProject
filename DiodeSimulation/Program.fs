module DiodeSimulation

open System
open System.IO
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
open DiodeHelper


let saturationCurrent = 1e-15
let vt = 0.025875

/// Creates the jacobian matrix
let createJacobian (circuitMat: DenseMatrix<float>) (conductDerivative: (int*int) list)(derivative: float) = 
    (circuitMat, conductDerivative)
    ||> List.fold(fun mat index -> let first = fst index
                                   let second = snd index
                                   let original = mat.[(first-1) , (second-1)]
                                   let sum = original + derivative
                                   mat.[(first-1) , (second-1)] <- sum
                                   mat)

/// Creates the voltage solution at an iteration
let createVoltageSol (nodenumber:int)= 
    let listmat = [1..nodenumber]
    let vec:DenseVector<float> = Vector.Create(nodenumber)
    (vec,listmat)
    ||> List.fold (fun mat el -> mat[(el-1)] <- 1.0
                                 mat)

/// Calculates the function vector 
let createFunctionVec (elements: float [])(conductanceList:((int*int)*float) list)(diodeCurrent: float)(voltage: DenseVector<float>)(currentdiodeel: int list) = 
    let sumVector = Vector.Create(elements)
    let sumVectorAdd = addOnVector sumVector currentdiodeel diodeCurrent
    let originalConductance = getMatrix conductanceList
    (originalConductance * voltage) - sumVectorAdd


/// Diode simulation
let diodeSimulation (data: DiodeData) = 
    
    let nodeNumber = getNodeNumber ( firstPart data.Conductance)
    
    // Iniital Voltage Solution (V1..Vn)
    let mutable (voltageSolution:DenseVector<float>) = 
        createVoltageSol nodeNumber
    
    let mutable voltageDiff = 10.0
    let conductanceList = data.Conductance

    while voltageDiff>0.0005 do 
        
        let (circuitMatrix:DenseMatrix<float>) = getMatrix conductanceList
        let vd = 
            match data.Vd with
            | (0,2) -> voltageSolution[1]
            | (0,1) -> voltageSolution[0]
            | (1,2) -> voltageSolution[0] - voltageSolution[1]
            | (2,1) -> voltageSolution[1] - voltageSolution[0] 
        //printfn "Vd: %A" (vd)
        let diodeCurrent = saturationCurrent * (exp(vd/vt) - 1.0)
        let derivative = (saturationCurrent/vt) * exp(vd/vt)
        let Jacobian = 
            createJacobian circuitMatrix data.ConductanceDerivative derivative
        
        let elements = data.Current
                       |> List.toArray
        let functionVector = createFunctionVec elements conductanceList diodeCurrent voltageSolution data.CurrentDiodeElement
    

        let jac_inv = Jacobian.GetInverse()
        let deltaVector = jac_inv * (-1.*functionVector)
        let nextVoltage = deltaVector + voltageSolution
        voltageDiff <- abs(voltageSolution[0] - nextVoltage[0])
        voltageSolution <- nextVoltage.ToDenseVector()