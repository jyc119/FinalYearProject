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
open FilesIO

open Node
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
//open MathNet.Numerics.LinearAlgebra

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

let private viewConductance (lst: ((int*int)*float) list) dispatch = 

    let list = 
        lst
        |> List.map (fun x -> let index = fst x
                              let firstindex = string(fst index)
                              let secondindex = string(snd index)
                              let str = firstindex + "," + secondindex
                              str , snd x)

    let makeInputLine (inputLabel : string, floatval: float) =
        let valueHandle =
            Input.number [
                Input.IsReadOnly true
                Input.DefaultValue <| sprintf "%f" floatval
                Input.Props [simulationNumberStyle]
            ]
        splittedLine (str <| makeIOLabel inputLabel 1) valueHandle
    div [] <| List.map makeInputLine list

let private viewAnalogInputs (state : (Component list * Connection list)) dispatch = 
    let componentList = (fst state)
                        |> List.map (fun x -> x.Type)
    let labelList = (fst state)
                    |> List.map (fun x -> x.Label)
    let componentLabellist = combineComponentIndexList labelList componentList
    let makeInputLine (inputLabel : string, component: ComponentType) =
        let value = 
            match component with
            | Resistor x | CurrentSource x | VoltageSource x -> 0.5
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
        |> List.map (fun x -> Some 0.0)
        

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

let cmd block =
    Elmish.Cmd.OfAsyncWith.perform block

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg

//ooga
let testingValueHandle (value:float)= 

    Input.number [
        Input.IsReadOnly true
        Input.DefaultValue <| sprintf "%f" value
        Input.Props [simulationNumberStyle]
    ]

let diagonalConductance (state: (Component list * Connection list)) model = 
    
    //-------Diagonal elements---------------
    let symbolModel = model.Sheet.Wire.Symbol

    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
        (Map.empty, connections)
        ||> List.fold (fun mapRes conn ->
                let symbol1 = getSymbol symbolModel conn.Port1.Id
                let symbol2 = getSymbol symbolModel conn.Port2.Id
                let connLabel = conn.Label
                match mapRes.TryFind connLabel with
                | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                | Some otherConnId -> mapRes
            )

    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                        
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (position,position) (symb1,symb2) mapRes)

        

    /// For each node(n) in the circuit, convert it into (n*n)*float list
    let DiagElements (map : Map<(int option * int option), (SymbolT.Symbol * SymbolT.Symbol)>) = 
        let mapping = 
            (Map.empty, map)
            ||> Map.fold(fun mapRes (pos1,pos2) (symbol1,symbol2) -> let coord1 = convertIntSome pos1
                                                                     let coord2 = convertIntSome pos2
                                                                     let resistance = symbolToResistance symbol1 symbol2
                                                                     let NonOption = convertFloatSome resistance
                                                                     Map.add (coord1,coord2) NonOption mapRes)

        mapping
        |> Map.toList
    
    (DiagElements mapNodeToSymbols)

let nonDiagonalConductance (state: (Component list * Connection list)) model = 
    // map each componentid to the connectionIDs

    let compIDSymbolMap = model.Sheet.Wire.Symbol.Symbols
    let connIDToWire = model.Sheet.Wire.Wires
    let symbolModel = model.Sheet.Wire.Symbol

    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )

    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                        
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (convertIntSome position) (symb1,symb2) mapRes)

    let tupleToList (lst : (SymbolT.Symbol*SymbolT.Symbol) list) = 
        lst
        |> List.collect(fun x -> [fst x; snd x])

    let isResistor (comp:SymbolT.Symbol) = 
        match comp.Component.Type with
        | Resistor x -> true
        | _ -> false
    
    let rec getIndex (lst : SymbolT.Symbol list) = 
        let rec tDup inL outL indexL = 
            match inL with
            | [] -> outL
            | el :: lst -> match isResistor el with
                           | true -> 
                                   let index = lst
                                               |> List.tryFindIndex (fun x -> el = x)
                                   match index with
                                   | Some x -> let res = (indexL, x+1+indexL),el
                                               tDup lst (res::outL) (indexL+1)
                                   | None -> tDup lst outL (indexL+1)
                           | false -> tDup lst outL (indexL+1)
    
        tDup lst [] 0
                          
    let listToTuple (lst:((int*int)*SymbolT.Symbol) list )= 
        lst
        |> List.map(fun x -> let a , b = fst x 
                             let newa = a/2
                             let newb = b/2
                             let resistance = match (snd x).Component.Type with
                                              | Resistor x -> -1.0/x
                             (newa, newb),resistance)
    
    let mergeTuple (keyListi : int list) (indexList: ((int*int)*float) list) = 
        indexList
        |> List.map(fun x -> let tup = fst x
                             let inte = snd x
                             
                             (keyListi[fst tup] , keyListi[snd tup]) , inte)
    
    let res (testMap: Map<int,(SymbolT.Symbol*SymbolT.Symbol)>) = 
    
        let keysOfMap = testMap.Keys
                        |> Seq.toList
    
        let valsOfMap = testMap.Values
                        |> Seq.toList
    
        valsOfMap
        |> tupleToList
        |> getIndex
        |> listToTuple
        |> mergeTuple keysOfMap

    res mapNodeToSymbols 

    
        
    (*
    
    let node1ToSymbols =
        (List.Empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun lisRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "N1" -> Some 1
                                                            | _ ->  None
                                                    
                                                        match position with 
                                                        | None -> lisRes
                                                        | Some x -> List.append lisRes [x,(symb1,symb2)])
    
    let getResistance = 
        let node = node1ToSymbols[0]
        let symb1 = fst (snd node)
        let symb2 = snd (snd node)
        match symb1.Component.Type, symb2.Component.Type with
        | Resistor x , _ -> -1.0/x
        | _ , Resistor x -> -1.0/x

    [(1,2) , getResistance]
    *)

let makeCurrentVec (state: (Component list * Connection list)) model = 

    let symbolModel = model.Sheet.Wire.Symbol

    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )
                                                              
    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                        
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (convertIntSome position) (symb1,symb2) mapRes)

    let getList = 
        (List.empty, mapNodeToSymbols)
        ||> Map.fold(fun lisRes nodeNum (sym1,sym2) -> match sym1.Component.Type , sym2.Component.Type with
                                                        | CurrentSource x , _ -> List.append [nodeNum, x] lisRes
                                                        | _ , CurrentSource x -> List.append [nodeNum, x] lisRes
                                                        | _ -> lisRes)

    let vecsize = 
        mapNodeToSymbols
        |> Map.toList
        |> List.length
 
    let index = fst getList[0]
    let floatval = snd getList[0]
    [for i in 0..(vecsize-1) -> if i = (index-1) then floatval else 0.0]

let getConductanceDerivative (state: (Component list * Connection list)) model = 
    
    let symbolModel = model.Sheet.Wire.Symbol

    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )
                                                              
    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                        
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (convertIntSome position) (symb1,symb2) mapRes)

    let listOfDiode = 
        (List.empty, mapNodeToSymbols)
        ||> Map.fold (fun lisRes nodeNum (symb1, symb2) -> match symb1.Component.Type , symb2.Component.Type with
                                                           | Diode , _ | _ , Diode -> List.append [nodeNum] lisRes
                                                           | _ -> lisRes)

    match List.length listOfDiode with
    | 1 -> [(listOfDiode[0],listOfDiode[0])]
    | 2 -> [(listOfDiode[0],listOfDiode[0]) ; (listOfDiode[0],listOfDiode[1]); (listOfDiode[1],listOfDiode[0]);(listOfDiode[1],listOfDiode[1])]

let getVd (state: (Component list * Connection list)) model = 
    let symbolModel = model.Sheet.Wire.Symbol
    
    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )
                                                                  
    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                            
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (convertIntSome position) (symb1,symb2) mapRes)

    let listOfDiode = 
        (List.empty, mapNodeToSymbols)
        ||> Map.fold (fun lisRes nodeNum (symb1, symb2) -> match symb1.Component.Type , symb2.Component.Type with
                                                           | Diode , _ | _ , Diode -> List.append [nodeNum] lisRes
                                                           | _ -> lisRes)
                                                           
    match List.length listOfDiode with
    | 1 -> (0,listOfDiode[0])
    | 2 -> (2,1)
                         
let getCurDiodeEl (state: (Component list * Connection list)) model = 
    let symbolModel = model.Sheet.Wire.Symbol
    
    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )
                                                                  
    let mapNodeToSymbols =
        (Map.empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun mapRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "" -> None
                                                            | _ ->  numberString label
                                                            
                                                        match position with 
                                                        | None -> mapRes
                                                        | Some x -> Map.add (convertIntSome position) (symb1,symb2) mapRes)

    let listOfDiode = 
        (List.empty, mapNodeToSymbols)
        ||> Map.fold (fun lisRes nodeNum (symb1, symb2) -> match symb1.Component.Type , symb2.Component.Type with
                                                           | Diode , CurrentSource x | CurrentSource x , Diode -> List.append [-nodeNum] lisRes
                                                           | Diode , Resistor x | Resistor x , Diode -> List.append [nodeNum] lisRes
                                                           | _ -> lisRes)

    match List.length listOfDiode with
    | 1 -> listOfDiode
           |> List.map (fun x -> -x)
    | 2 -> listOfDiode
    

let linearSimulation (state: (Component list * Connection list)) model = 
    //-------Diagonal elements---------------
    let diagonalElements = diagonalConductance state model
    let nonDiagElements = nonDiagonalConductance state model

    let currentS = (fst state)
                  |> List.find (fun x -> match x.Type with
                                         | CurrentSource x -> true
                                         | _ -> false)

    let current =                                         
        match currentS.Type with
        | CurrentSource x -> x
        | _ -> 0.0

    {
    Conductance = diagonalElements @ nonDiagElements
    Current = current
    }

let diodeSimulation (state: (Component list * Connection list)) model = 
    let diagonalElements = diagonalConductance state model
    let nonDiagElements = nonDiagonalConductance state model

    let conductance = diagonalElements @ nonDiagElements
    let vd = getVd state model
    let currentdiodeel = getCurDiodeEl state model
    let conductancederi = getConductanceDerivative state model
    let current = makeCurrentVec state model

    {
    Conductance = conductance
    ConductanceDerivative = conductancederi
    Current = current
    CurrentDiodeElement = currentdiodeel
    Vd = vd
    }
    

let transistorSimulation (state: (Component list * Connection list)) model = 

    let symbolModel = model.Sheet.Wire.Symbol

    let  mapConnToSymbols
        (connections : Connection list)
        : Map<string,(SymbolT.Symbol * SymbolT.Symbol)> =
            (Map.empty, connections)
            ||> List.fold (fun mapRes conn ->
                    let symbol1 = getSymbol symbolModel conn.Port1.Id
                    let symbol2 = getSymbol symbolModel conn.Port2.Id
                    let connLabel = conn.Label
                    match mapRes.TryFind connLabel with
                    | None -> mapRes.Add (connLabel, (symbol1 , symbol2))
                    | Some otherConnId -> mapRes
                )

    let node1ToSymbols =
        (List.Empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun lisRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "N1" -> Some 1
                                                            | _ ->  None
                                                    
                                                        match position with 
                                                        | None -> lisRes
                                                        | Some x -> List.append lisRes [x,(symb1,symb2)])

    let node2ToSymbols =
        (List.Empty, mapConnToSymbols (snd state))
        ||> Map.fold (fun lisRes label (symb1,symb2) -> let position = 
                                                            match label with 
                                                            | "N2" -> Some 2
                                                            | _ ->  None
                                                    
                                                        match position with 
                                                        | None -> lisRes
                                                        | Some x -> List.append lisRes [x,(symb1,symb2)])

    let getResistance (nodetosymbol : (int * (SymbolT.Symbol * SymbolT.Symbol)) list) = 
        let node = nodetosymbol[0]
        let symb1 = fst (snd node)
        let symb2 = snd (snd node)
        match symb1.Component.Type, symb2.Component.Type with
        | Resistor x , _ -> x
        | _ , Resistor x -> x

    let getVdd (connections : Connection list) = 
         (List.empty, connections)
         ||> List.fold(fun lisRes conn -> let symbol1 = getSymbol symbolModel conn.Port1.Id
                                          let symbol2 = getSymbol symbolModel conn.Port2.Id
                                          
                                          match symbol1.Component.Type , symbol2.Component.Type with
                                          | VoltageSource x , Resistor y 
                                          | Resistor y , VoltageSource x -> List.append lisRes [x]
                                          | _ -> lisRes)

    let getVoltageSource (connections : Connection list) = 
        (List.empty, connections)
        ||> List.fold(fun lisRes conn -> let symbol1 = getSymbol symbolModel conn.Port1.Id
                                         let symbol2 = getSymbol symbolModel conn.Port2.Id
                                         
                                         match symbol1.Component.Type , symbol2.Component.Type with
                                         | VoltageSource x , Transistor
                                         | Transistor , VoltageSource x -> List.append lisRes [x]
                                         | _ -> lisRes)

    let vdd = (getVdd (snd state))[0]
    let voltagesource = (getVoltageSource (snd state))[0]
    let collector = getResistance node2ToSymbols
    let emitter = getResistance node1ToSymbols

    {
    Vdd = vdd
    Voltage = voltagesource
    ResistorCollector = collector
    ResistorEmitter = emitter
    }

let extractACSimulation (state: (Component list * Connection list)) = 

    let extractValue (comp:Component) = 
        match comp.Type with
        | Resistor x -> x
        | Capacitor x -> x
        | VoltageSource x -> x

    let resistance = 
        (fst state) 
        |> List.find (fun x -> match x.Type with
                               | Resistor x -> true
                               | _ -> false)
        |> extractValue                         

    let capacitance = 
        (fst state) 
        |> List.find (fun x -> match x.Type with
                               | Capacitor x -> true
                               | _ -> false) 
        |> extractValue

    let voltage = 
        (fst state) 
        |> List.find (fun x -> match x.Type with
                               | VoltageSource x -> true
                               | _ -> false) 
        |> extractValue

    {
        Voltage = voltage
        Capacitance = capacitance
        Resistance = resistance
    }

let private writeToAndReadConductanceFile folderPath fileName fileNameExt data = 
    saveConductanceToFile folderPath fileName data |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName
    
    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadConductanceFromPathCheck
    //
    match resultFloat with 
    | Ok x -> 10
    | Error str -> 0


let private writeToAndReadDiodeFile folderPath fileName fileNameExt data = 

    saveDiodeDataToFile folderPath fileName data |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName

    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadDiodeFromPathCheck
    //
    match resultFloat with 
    | Ok x -> 10
    | Error str -> 0

let private writeToAndReadTransFile folderPath fileName fileNameExt data = 

    saveTransDataToFile folderPath fileName data |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName

    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadTransFromPathCheck
    //
    match resultFloat with 
    | Ok x -> 10
    | Error str -> 0
    
let private writeToAndReadACFile folderPath fileName fileNameExt data = 

    saveACDataToFile folderPath fileName data |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName

    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadACFromPathCheck
    //
    match resultFloat with 
    | Ok x -> 10
    | Error str -> 0

let private whichSimulation (state: (Component list * Connection list)) = 

    let isAC = 
        (fst state)
        |> List.map (fun x -> match x.Type with
                              | Capacitor y -> true
                              | _ -> false)
        |> List.contains true

    let isTrans = 
        (fst state)
        |> List.map (fun x -> match x.Type with
                              | Transistor -> true
                              | _ -> false)
        |> List.contains true

    let isDiode = 
        (fst state)
        |> List.map (fun x -> match x.Type with
                              | Diode -> true
                              | _ -> false)
        |> List.contains true 
        
    match isAC with
    | true -> "AC"
    | false -> match isTrans with
               | true -> "Trans"
               | false -> match isDiode with
                          | true -> "Diode"
                          | false -> "Conductance"


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

    let simMode = whichSimulation state

    let simData = 
        match simMode with
        | "Conductance" -> let folderPath = "D:\FYP\CommunicationValue"
                           let fileName = "LinearSimulationData"
                           let fileNameExt = "LinearSimulationData.dgm"

                           let data = linearSimulation state model 
                           writeToAndReadConductanceFile folderPath fileName fileNameExt data

        | "Diode" -> let folderPath = "D:\FYP\CommunicationValue"
                     let fileName = "DiodeSimulationData"
                     let fileNameExt = "DiodeSimulationData.dgm"

                     let data = diodeSimulation state model 
                     writeToAndReadDiodeFile folderPath fileName fileNameExt data

        | "Trans" -> let folderPath = "D:\FYP\CommunicationValue"
                     let fileName = "TransistorSimulationData"
                     let fileNameExt = "TransistorSimulationData.dgm"

                     let data = transistorSimulation state model 
                     writeToAndReadTransFile folderPath fileName fileNameExt data

        | "AC" -> let folderPath = "D:\FYP\CommunicationValue"
                  let fileName = "ACSimulationData"
                  let fileNameExt = "ACSimulationData.dgm"

                  let data = extractACSimulation state  
                  writeToAndReadACFile folderPath fileName fileNameExt data
    
    (*
    let questionIcon = str "\u003F"
    let folderPath = "D:\FYP\CommunicationValue"
    let fileName = "diodedata"
    let fileNameExt = "diodedata.dgm"
    let combine = "D:\FYP\CommunicationValue\ff"
    let testDiode = {
        Conductance = [(1,1),0.1;(2,2),0.1;(1,2),-0.1]
        ConductanceDerivative = [(2,2)]
        Current = [0.1;0.0]
        CurrentDiodeElement = [-2]
        Vd = (0,2)
    }
    *)
    (*
    //---- Linear simulation -----
    saveConductanceToFile folderPath fileName lst |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName
    
    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadConductanceFromPathCheck
    //
    let floatFromPath = 
        match resultFloat with 
        | Ok x -> x
        | Error str -> [(10,10),12]
    *)

    (*
    //----- Diode simulation ------
    saveDiodeDataToFile folderPath fileName testDiode |> ignore
    removeFileWithExtn ".dgmauto" folderPath fileName

    let filePath = path.join [| folderPath; fileNameExt |]
    let resultFloat =  filePath |> tryLoadDiodeFromPathCheck
    //
    let floatFromPath = 
        match resultFloat with 
        | Ok x -> Some x
        | Error str -> None
    *)


    //let listofnondiag = nonDiagonalConductance state model
    //let simulationResults = linearSimulation state model


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

        (*
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Current" ]
        viewAnalogInputs
            state
            dispatch
        *)
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Voltages" ]
        viewVoltages
            model
            (snd state)
            dispatch
        (*
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Testing" ]
        viewConductance
            simulationResults
            dispatch
        *)
        (*
        Heading.h5 [ Heading.Props [ Style [ MarginTop "15px" ] ] ] [ str "Testing" ]
        splittedLine (str <| makeIOLabel simMode 1) (testingValueHandle 2.1)
        *)
        //splittedLine (str <| makeIOLabel "fstindex" 1) (testingValueHandle sndindex)
        // splittedLine (str <| makeIOLabel "Comms value" 1) (testingValueHandle floatFromPath)
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

        let mainSim = 

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
        mainSim
    
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
