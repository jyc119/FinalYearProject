(*
    Simulator.fs

    This module collects all the APIs required for a simulation. 
*)

module Simulator

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open Fable.Core
open Node.ChildProcess

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

//---Canvas Map Helper functions------

//----Matrix helper functions---------
(*
/// Builds a nxn matrix with zero indexes
let buildMatrix (nodeNumber : int) : DenseMatrix<float> = 
    Matrix.Create(nodeNumber, nodeNumber)

/// Obtain the top left element in the matrix
let topLeftElement (matrix : DenseMatrix<float>) : float = 
    matrix.[0,0]    

let matrixInverse (matrix : DenseMatrix<float>) : Matrix<float> = 
    matrix.GetInverse()

/// Takes the voltage list and input it at position [n,n]
let insertDiagonals (nodeNumber : int) (voltageList : float option list) : DenseMatrix<float> = 

    let matrix = buildMatrix nodeNumber

    let voltages = 
        voltageList
        |> List.map convertSome
    

    for i in 0..(nodeNumber-1) do
        matrix.[i,i] <- voltages[i]

    matrix

let insertNonDiagonals (matrix : DenseMatrix<float>) (list : ((int*int) * float) list ) = 
    (matrix , list)
    ||> List.fold (fun mat lst -> let index = fst lst
                                  let value = snd lst
                                  let firstindex = fst index
                                  let secondindex = snd index
                                  mat.[(firstindex-1),(secondindex-1)] <- value
                                  mat.[(secondindex-1),(firstindex-1)] <- value
                                  mat
                          )
*)
//let insertNonDiagonals (matrix : DenseMatrix<float>) = 

//--------Non-Linear Circuit Implementation------------------

let saturationCurrent = 1e-15
let vt = 0.025875
//let vd = initialVoltageSolution[0,1]
//let diodeCurrent = saturationCurrent * (exp(vd/vt) - 1.0)
let expeirnec = exp 1.0

//------Connection Helper functions--------

/// Lookup the width of a connection in the connectionsWidth map or fail.
let getConnectionWidth
        (connectionsWidth : ConnectionsWidth)
        (connId : ConnectionId)
        : int option =
    match connectionsWidth.TryFind connId with
    | None -> failwithf "what? getConnectionWidth received inexistent connectionId: %A" connId
    | Some width -> width

//--- RUn Executable-----
(*
let export = new IExports
let executablePath = "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe"
let child = export.execFile(executablePath, )

let executablePath = "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe"
*)

//-----AC function--------

let checkIfVoltage (comp:Component) : bool = 
    match comp.Type with
    | VoltageSource x -> true
    | _ -> false

let checkIfCapacitor (comp:Component) : bool = 
    match comp.Type with
    | Capacitor x -> true
    | _ -> false

let checkIfResistor (comp:Component) : bool = 
    match comp.Type with
    | Resistor x -> true
    | _ -> false

let getValFromComp (comp: Component) = 
    match comp.Type with
    | VoltageSource x -> x
    | Resistor x -> x
    | Capacitor x -> x


let getRCFilter (canvas: CanvasState) = 
    let componentList = fst canvas
    
    let voltage = 
        let voltageSource = 
            componentList
            |> List.filter checkIfVoltage

        getValFromComp voltageSource[0]

    let resistor = 
        let resistance = 
            componentList
            |> List.filter checkIfResistor

        getValFromComp resistance[0]

    let capacitance = 
        let cap = 
            componentList
            |> List.filter checkIfCapacitor

        getValFromComp cap[0]

    {
        Resistance = resistor
        Capacitance = capacitance
        Voltage = voltage
    }
