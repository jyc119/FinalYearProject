//(*
//    SimulationView.fs
//
//    View for simulation in the right tab.
//*)
//
module SimulationView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props

open NumberHelpers
open Helpers
open TimeHelpers
open JSHelpers
open DiagramStyle
open Notifications
open PopupView
open MemoryEditorView
open ModelType
open CommonTypes
open SimulatorTypes
open Extractor
open Simulator
open Symbol
open Sheet.SheetInterface
open DrawModelType

open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
//open MathNet.Numerics.LinearAlgebra

//----------------------------View level simulation helpers------------------------------------//
(*
type SimCache = {
    Name: string
    StoredState: CanvasState
    StoredResult: Result<SimulationData, SimulationError>
    }



let simCacheInit name = {
    Name = name; 
    StoredState = ([],[]) // reduced canvas state from extractReducedState
    StoredResult = Ok {
        FastSim = FastCreate.emptyFastSimulation()
        Graph = Map.empty 
        Component = []
        IsSynchronous=false
        NumberBase = NumberBase.Hex
        ClockTickNumber = 0
        }
    }

/// Used to store last canvas state and its simulation
let mutable simCache: SimCache = simCacheInit ""

/// Start up a simulation, doing all necessary checks and generating simulation errors
/// if necesary. The code to do this is quite long so results are memoized. this is complicated because
/// we want the comparison (in the case nothing has chnaged) to be fast.
/// 1. If the current sheet changes we redo the simulation. 
/// 2. While current sheet does not change we assume the other sheets
/// ( and so subsheet content) cannot change. 
/// 3. Therefore we need only compare current sheet canvasState with its
/// initial value. This is compared using extractReducedState to make a copy that has geometry info removed 
/// from components and connections.


let rec prepareSimulationMemoized
        (diagramName : string)
        (canvasState : CanvasState)
        (loadedDependencies : LoadedComponent list)
        : Result<SimulationData, SimulationError> * CanvasState =
    let rState = extractReducedState canvasState
    if diagramName <> simCache.Name then
        simCache <- simCacheInit diagramName
        // recursive call having initialised the cache state on sheet change
        prepareSimulationMemoized diagramName canvasState loadedDependencies
    else
        let isSame = rState = simCache.StoredState
        if  isSame then
            simCache.StoredResult, rState
        else
            printfn "New simulation"
            let simResult = prepareSimulation diagramName rState loadedDependencies
            simCache <- {
                Name = diagramName
                StoredState = rState
                StoredResult = simResult
                }
            simResult, rState
 

 
/// Start simulating the current Diagram.
/// Return SimulationData that can be used to extend the simulation
/// as needed, or error if simulation fails.
/// Note that simulation is only redone if current canvas changes.
let makeSimData model =
    let start = TimeHelpers.getTimeMs()
    match model.Sheet.GetCanvasState(), model.CurrentProj with
    | _, None -> None
    | canvasState, Some project ->
        let otherComponents = 
            project.LoadedComponents 
            |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        (canvasState, otherComponents)
        ||> prepareSimulationMemoized project.OpenFileName
        |> Some
        |> TimeHelpers.instrumentInterval "MakeSimData" start
*)
let changeBase dispatch numBase = numBase |> SetSimulationBase |> dispatch

/// A line that can be used for an input, an output, or a state.
let private splittedLine leftContent rightConent =
    Level.level [Level.Level.Props [Style [MarginBottom "10px"]]] [
        Level.left [] [
            Level.item [] [ leftContent ]
        ]
        Level.right [] [
            Level.item [] [ rightConent ]
        ]
    ]

/// Pretty print a label with its width.
let private makeIOLabel label width =
    let label = cropToLength 15 true label
    match width with
    | 1 -> label
    | w -> sprintf "%s (%d bits)" label w

let private combineComponentIndexList (list1:string list) (list2: ComponentType list) = 
    list1 
    |> List.mapi (fun i x -> (x, list2[i]))

let private combineVoltageIndexList (list1:string list) (list2: float list) = 
    list1 
    |> List.mapi (fun i x -> (x, list2[i]))

/// return output port data from simulation
let rec extractFastSimulationOutput
    (fs: FastSimulation)
    (step: int)
    ((cid, ap): ComponentId * ComponentId list)
    (opn: OutputPortNumber) : WireData =
    
   let (OutputPortNumber n) = opn
   match Map.tryFind (cid, ap) fs.FComps with
   | Some fc ->
        //printfn $"Extracting port {opn} from {fc.FullName} in step {step}"
        match Array.tryItem (step % fs.MaxArraySize) fc.Outputs[n].Step with
        | None -> failwithf $"What? extracting output {n} in step {step} from {fc.FullName} failed with clockTick={fs.ClockTick}"
        | Some fd -> fd
        |> (fun fd -> 
                if fd.Width=0 then failwithf $"Can't find valid data in step {step}:index{step % fs.MaxArraySize} from {fc.FullName} with clockTick={fs.ClockTick}"
                fd |> fastToWire)
   | None ->
        // if it is a custom component output extract from the corresponding Output FastComponent
        match Map.tryFind ((cid, ap), opn) fs.G.CustomOutputLookup with
        | Some (cid, ap) -> extractFastSimulationOutput fs step (cid, ap) (OutputPortNumber 0)
        | None -> failwithf "What? extracting component data failed - can't find component from id"

/// Extract top-level inputs or outputs with names and wire widths. Used by legacy code.
let extractFastSimulationIOs
    (simIOs: SimulationIO list)
    (simulationData: SimulationData)
    : (SimulationIO * WireData) list =
    let fs = simulationData.FastSim
    //let inputs = simulationData.Inputs

    simIOs
    |> List.map
        (fun ((cid, label) as io) ->
            let wd = extractFastSimulationOutput fs simulationData.ClockTickNumber (cid, []) (OutputPortNumber 0)
            //printfn $"Extrcating: {io} --- {wd}"
            io, wd)

let private viewAnalogInputs (state : (Component list * Connection list)) dispatch = 
    let componentList = (fst state)
                        |> List.map (fun x -> x.Type)
    let labelList = (fst state)
                    |> List.map (fun x -> x.Label)
    let componentLabellist = combineComponentIndexList labelList componentList
    let makeInputLine (inputLabel : string, component: ComponentType) =
        let value = 
            match component with
            | Resistor x | CurrentSource x | VoltageSource x -> x
            | _ -> 4.8
        let valueHandle =
            Input.number [
                Input.IsReadOnly true
                Input.DefaultValue <| sprintf "%f" value
                Input.Props [simulationNumberStyle]
            ]
        splittedLine (str <| makeIOLabel inputLabel 1) valueHandle
    div [] <| List.map makeInputLine componentLabellist

let private viewVoltages model (conn : Connection list) dispatch = 

    let symbolModel = model.Sheet.Wire.Symbol
        
    let getDiagonalVoltage (connection : Connection) : float = 
        let symbol1 , symbol2 = 
            getSymbol symbolModel connection.Port1.Id , getSymbol symbolModel connection.Port2.Id
        let portType1 , portType2 = 
            symbol1.Component.Type , symbol2.Component.Type

        match portType1,portType2 with
        | Resistor x , Resistor y -> 1.0/x + 1.0/y
        | Resistor x , _ | _ , Resistor x -> 1.0/x
        |  _ -> 0

    let diagonalVoltages = 
        conn
        |> List.map getDiagonalVoltage
        |> List.map (fun x -> Some x)
        

    //let matrixElement = testingMatrix newVoltageList
    let voltageLabellist = makeVoltageLabelList conn diagonalVoltages

    let makeInputLine(inputLabel : string, voltage: float) = 
        (*
        let value = 
            match voltage with
            | Some x -> x
            | None -> 0
        *)
        let valueHandle =
            Input.number [
                Input.IsReadOnly true
                Input.DefaultValue <| sprintf "%f" voltage
                Input.Props [simulationNumberStyle]
            ]
        splittedLine (str <| makeIOLabel inputLabel 1) valueHandle
    div [] <| List.map makeInputLine voltageLabellist

let viewSimulationError (simError : SimulationError) =
    let error = 
        match simError.InDependency with
        | None ->
            div [] [
                str simError.Msg
                br []
                str <| "Please fix the error and retry."
            ]
        | Some dep ->
            div [] [
                str <| "Error found in dependency \"" + dep + "\":"
                br []
                str simError.Msg
                br []
                str <| "Please fix the error in the dependency and retry."
            ]
    div [] [
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Errors" ]
        error
    ]

let private simulationClockChangePopup (simData: SimulationData) (dispatch: Msg -> Unit) (dialog:PopupDialogData) =
    let step = simData.ClockTickNumber
    div [] 
        [
            h6 [] [str $"This simulation contains {simData.FastSim.FComps.Count} components"]
            (match dialog.Int with 
            | Some n when n > step -> 
                Text.p [
                    Modifiers [Modifier.TextWeight TextWeight.Bold] 
                  ] [str "Goto Tick:"]
            | _ -> Text.p [
                            Modifiers [
                                Modifier.TextWeight TextWeight.Bold
                                Modifier.TextColor IsDanger] 
                          ] [str $"The clock tick must be > {step}"])
            br []
            Input.number [
                Input.Props [AutoFocus true;Style [Width "100px"]]
                Input.DefaultValue <| sprintf "%d" step
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]

        ]
(*
let simulateWithTime steps simData =
    let startTime = getTimeMs()
    FastRun.runFastSimulation (steps + simData.ClockTickNumber) simData.FastSim 
    getTimeMs() - startTime
*)
let cmd block =
    Elmish.Cmd.OfAsyncWith.perform block

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg

//let oogabooga (nodeNumber : int) : DenseMatrix<float> = 
 //   Matrix.Create(nodeNumber,nodeNumber) 
(*
let siu (matrix : DenseMatrix<float>) : float = 
    matrix.[0,0]
*)
let monka (voltageList : float list) : float =
    let nodeNumber = List.length voltageList
    float(nodeNumber)
(*
let really = 
    let aLU : DenseMatrix<float> = 
        Matrix.Create(2,2)
    aLU.[0,0]
*)
//ooga
let testingValueHandle = 

    //let wrong = Simulator.value
    let something = monka [2.34;4.1]
    let ooga = 2
    Input.number [
        Input.IsReadOnly true
        Input.DefaultValue <| sprintf "%f" something
        Input.Props [simulationNumberStyle]
    ]

let private viewSimulationData (state: (Component list * Connection list)) model dispatch =
    (*      
    let maybeStatefulComponents() =
        let stateful = 
            FastRun.extractStatefulComponents simData.ClockTickNumber simData.FastSim
            |> Array.toList
        match List.isEmpty stateful with
        | true -> div [] []
        | false -> div [] [
            Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Stateful components" ]
            viewStatefulComponents step stateful simData.NumberBase model dispatch
        ]
    *)
    let questionIcon = str "\u003F"

    let tip tipTxt txt =
        span [
                // Style [Float FloatOptions.Left]
                HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                Tooltip.dataTooltip tipTxt
            ]
            [
                Text.span [
                    Modifiers [
                        Modifier.TextColor IsPrimary
                    ]
                    Props [
                        Style [
                            Display DisplayOptions.InlineBlock
                            Width "80px"
                            TextAlign TextAlignOptions.Center]]
            ] [str txt] ]
    let test = Input.number [
                  Input.IsReadOnly true
                  Input.DefaultValue <| sprintf "%f" 20.5
                  Input.Props [simulationNumberStyle]
               ]
    div [] [

        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Inputs" ]
        viewAnalogInputs
            state
            dispatch
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Voltages" ]
        viewVoltages
            model
            (snd state)
            dispatch
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Testing" ]
        splittedLine (str <| makeIOLabel "Testing" 1) testingValueHandle
        //maybeStatefulComponents()
    ]

let SetSimErrorFeedback (simError:SimulatorTypes.SimulationError) (model:Model) (dispatch: Msg -> Unit) =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let keyDispatch = SheetT.KeyPress >> sheetDispatch
    if simError.InDependency.IsNone then
        // Highlight the affected components and connection only if
        // the error is in the current diagram and not in a
        // dependency.
        let (badComps,badConns) = (simError.ComponentsAffected, simError.ConnectionsAffected)
        dispatch <| SetHighlighted (badComps,badConns)
        if not (Sheet.isAllVisible model.Sheet badConns badComps) then
            // make whole diagram visible if any of the errors are not visible
            keyDispatch <| SheetT.KeyboardMsg.CtrlW
        dispatch <| Sheet(SheetT.SetWaveSimMode false)


let viewSimulation model dispatch =
    let state = model.Sheet.GetCanvasState ()
    // let JSState = model.Diagram.GetCanvasState ()
    let startSimulation () =
        match state, model.CurrentProj with
        | _, None -> failwith "what? Cannot start a simulation without a project"
        | canvasState, Some project ->
            (*
            let otherComponents =
                project.LoadedComponents
                |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
            simCache <- simCacheInit ""
            (canvasState, otherComponents)
            ||> prepareSimulationMemoized project.OpenFileName
            |> function
               | Ok (simData), state -> Ok simData
               | Error simError, state ->
                  printfn $"ERROR:{simError}"
                  SetSimErrorFeedback simError model dispatch
                  Error simError
            *)
            StartSimulation
            |> dispatch
            let sheetName = project.OpenFileName
            match Map.tryFind sheetName (fst model.WaveSim) with
            | Some wSModel ->
                printfn "Closing wavesim..."
                dispatch <| SetWSMod {wSModel with InitWaveSimGraph=None; WSViewState=WSClosed; WSTransition = None}
                dispatch <| SetWaveSimIsOutOfDate true
            | None -> ()
    let test = model.CurrentStepSimulationStep
    match model.CurrentStepSimulationStep with
    | false ->
        //let simRes = makeSimData model
        //let isSync = match simRes with | Some( Ok {IsSynchronous=true},_) | _ -> false
        let buttonColor, buttonText = 
            //match simRes with
            //| None -> IColor.IsWhite, ""
            //| Some (Ok _, _) -> 
            IsSuccess, "Start Simulation"
            //| Some (Error _, _) -> IsWarning, "See Problems"
        div [] [
            str "Simulate simple logic using this tab."
            br []
            //str (if isSync then "You can also use the Waveforms >> button to view waveforms" else "")
            br []; br []
            Button.button
                [ 
                    Button.Color buttonColor; 
                    Button.OnClick (fun _ -> startSimulation()) ; 
                ]
                [ str buttonText ]
        ]
    
    | true ->
        let body = (*
                    match sim with
                    | Error simError -> viewSimulationError simError
                    | Ok simData -> 
                    *)
                    viewSimulationData state model dispatch
        let endSimulation _ =
            dispatch CloseSimulationNotification // Close error notifications.
            dispatch <| Sheet (SheetT.ResetSelection) // Remove highlights.
            dispatch EndSimulation // End simulation.
            dispatch <| (JSDiagramMsg << InferWidths) () // Repaint connections.
        div [] [
            Button.button
                [ Button.Color IsDanger; Button.OnClick endSimulation ]
                [ str "End simulation" ]
            br []; br []
            str "The simulation uses the diagram as it was at the moment of
                    pressing the \"Start simulation\" button."
            hr []
            body
        ]
