open System
open System.IO
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
open TransistorHelper


/// Get conductance matrix
let getConductance (collectorR: float) (emitterR: float) = 
    let m = Matrix.Create(2,2)
    m.[0,0] <- (1.0/emitterR)
    m.[1,1] <- (1.0/collectorR)
    m

/// Creates the Jacobian Matrix
let createJacob (jacob_expression:float) (conductanceMat:DenseMatrix<float>) = 
    let jacob = Matrix.Create(2,2)
    let coord00 = conductanceMat.[0,0]
    let coord11 = conductanceMat.[1,1]
    let beta = 100.0

    let sum0 = coord00 + (jacob_expression*(1.0+1.0/beta))
    let sum1 = coord11 + (-jacob_expression)
    jacob.[0,0] <- sum0
    jacob.[1,0] <- sum1
    jacob.[1,1] <- conductanceMat.[1,1]
    jacob

/// Transistor Simulation
let transistorSimulation (data: TransistorSim) = 
    let beta = 100.0
    let saturationCurrent = 1e-15
    let Vt = 0.025875
    let mutable voltageDiff = 10.0
    let conductanceMatrix = getConductance data.ResistorCollector data.ResistorEmitter
    let mutable (voltageSolution:DenseVector<float>) = 
        let vec = Vector.Create(4.0,13.0)
        vec

    while voltageDiff>0.0005 do 
    
        let vbe = 5.0 - voltageSolution[0]
        let jacob_expression = (saturationCurrent/Vt) * exp(vbe/Vt)
    
        let Ie = saturationCurrent*(1.0+1.0/beta)*exp(vbe/Vt)
        let Ic = saturationCurrent*exp(vbe/Vt)
    
        let Jacob = 
            createJacob jacob_expression conductanceMatrix
    
        let currentVec = Vector.Create(Ie,-Ic+(20.0/data.ResistorCollector))
        let functionVector = (conductanceMatrix * voltageSolution) - currentVec
    
        let jac_inv = Jacob.GetInverse()
        let neg = (-1.*functionVector)
        let deltaVector = jac_inv * neg
        let nextVoltage = deltaVector + voltageSolution
        voltageDiff <- abs(voltageSolution[0] - nextVoltage[0])
        voltageSolution <- nextVoltage.ToDenseVector()