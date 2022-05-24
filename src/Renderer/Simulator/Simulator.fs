(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open CommonTypes
open NumberHelpers
open SimulatorTypes

open SimulationBuilder
open SimulationRunner
open DependencyMerger
open SimulationGraphAnalyser

open System
open System.IO
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
//open FsAlg.Generic

// Simulating a circuit has four phases (not precisely in order of execution):
// 1. Building a simulation graph made of SimulationComponents.
// 2. Merging all the necessary dependencies.
// 3. Analyse the graph to look for errors, such as unconnected ports,
//    combinatorial loops, etc...
// 4. Setting the values of the input nodes of the graph to kickstart the
//    simulation process.

/// Builds the graph and simulates it with all inputs zeroed.

(*
let rec prepareSimulation
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> =

    /// Tune for performance of initial zero-length simulation versus longer run.
    /// Probably this is not critical.
    let initMaxSteps = 10
    match runCanvasStateChecksAndBuildGraph canvasState loadedDependencies with
    | Error err -> Error err
    | Ok graph ->
        match mergeDependencies diagramName graph
                                canvasState loadedDependencies with
        | Error err -> Error err
        | Ok graph ->
            // Simulation graph is fully merged with dependencies.
            // Perform checks on it.
            let components, connections = canvasState
            let analog = getSimulationIOs components
            match analyseSimulationGraph diagramName graph connections with
            | Some err -> Error err
            | None -> 
                try
                    match FastRun.buildFastSimulation initMaxSteps graph with
                    | Ok fs -> 
                        Ok {
                            FastSim = fs                           
                            Graph = graph // NB graph is now not initialised with data
                            Component = analog
                            IsSynchronous = hasSynchronousComponents graph
                            NumberBase = Hex
                            ClockTickNumber = 0
                        }
                    | Error  e -> Error  e
                with
                | e -> 
                    printfn "\nEXCEPTION:\n\n%A\n%A\n\n" e.Message e.StackTrace
                    Error {
                        Msg = sprintf "\nInternal ERROR in Issie fast simulation: %A\n\n%A\n" e.Message e.StackTrace
                        InDependency = None
                        ComponentsAffected = []
                        ConnectionsAffected = []
                    }
                |> Result.map (fun sd ->
                    //Fast.compareFastWithGraph sd |> ignore
                    sd)
*)

(*
/// Expose the feedSimulationInput function from SimulationRunner.
let feedSimulationInput = SimulationRunner.feedSimulationInput

/// Expose the feedClockTick function from SimulationRunner.
let feedClockTick = SimulationRunner.feedClockTick

/// Expose the extractSimulationIOs function from SimulationRunner.
let extractSimulationIOs = SimulationRunner.extractSimulationIOs

/// Get some info and the state of all stateful components in a graph.
let extractStatefulComponents
        (graph : SimulationGraph)
        : SimulationComponent list =
    graph
    |> Map.toList
    |> List.map snd
    |> List.filter (fun comp -> comp.State <> NoState)
    // TODO: recursively search custom components?
*)
(*
let buildMatrix (nodeNumber : int) : DenseMatrix<float> = 
    Matrix.Create(nodeNumber, nodeNumber)

let topLeftElement (matrix : DenseMatrix<float>) : float = 
    matrix.[0,0]

let testingMatrix (voltageList : float list) =
    let nodeNumber = List.length voltageList
    let matrix = buildMatrix nodeNumber
    topLeftElement matrix

let value = testingMatrix [2.3;4.4]


let path = "D:\HLP\Testing\myTest.txt"

let main =
    let x = ["ooga";"booga"]
    //let y = List.map System.Math.Sqrt x
     
    let xprecision = 3
    let yprecision = 5
     
    let (file : StreamWriter) = System.IO.File.CreateText(path)
    let line = sprintf "%s"
    List.iter (fun x -> file.WriteLine (line x)) x

let sr = new StreamReader(path)
let reader = Console.WriteLine(sr.ReadToEnd())

printfn "Really testing: %A" (reader)
*)

//---Helper Functions------
/// Check if circuit contains nonlinear components
let isNonLinearCircuit (components : Component list) : bool = 

    let compTypes = 
        components
        |> List.map(fun x -> x.Type)

    compTypes
    |> List.contains Diode

/// Check if ground is present in the list
let isGroundPresent (components : Component list) : bool =

    let compTypes = 
        components
        |> List.map(fun x -> x.Type)

    compTypes
    |> List.contains Ground

///Make voltage and label list for simulation
let makeVoltageLabelList (conn : Connection list) (voltageList : float option list) = 

    let labelList = conn
                    |> List.map (fun x -> x.Label)
    
    let newVoltageList , newLabelList = reduceStringFloatList labelList voltageList
    //let matrixElement = testingMatrix newVoltageList

    combineVoltageIndexList newLabelList newVoltageList

//----Matrix helper functions---------

/// Builds a nxn matrix with zero indexes
let buildMatrix (nodeNumber : int) : DenseMatrix<float> = 
    Matrix.Create(nodeNumber, nodeNumber)

/// Obtain the top left element in the matrix
let topLeftElement (matrix : DenseMatrix<float>) : float = 
    matrix.[0,0]    

/// Takes the voltage list and input it at position [n,n]
let insertDiagonals (nodeNumber : int) (voltageList : float option list) : DenseMatrix<float> = 

    let matrix = buildMatrix nodeNumber

    let voltages = 
        voltageList
        |> List.map convertSome
    

    for i in 0..(nodeNumber-1) do
        matrix.[i,i] <- voltages[i]

    matrix

let insertNonDiagonals (matrix : DenseMatrix<float>) = 
    

(*
/// From the number of nodes, build an nxn matrix
let LinearSimulation (model: DrawModelType.Model) (voltageList : float list) =
    let nodeNumber = List.length voltageList
    let matrix = buildMatrix nodeNumber
    topLeftElement matrix
*)