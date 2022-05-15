module WaveSimHelpers

open Fable.React
open Fable.React.Props

open Fulma
open TimeHelpers
open ModelType
open DiagramStyle
open CommonTypes
open FileMenuView
open Extractor
open SimulatorTypes
open Sheet.SheetInterface
open DrawModelType

let maxLastClk = 500u

type WaveGapT = {
    /// length of stable waveform in cycles
    GapLen: int
    /// starting cycle of stable waveform
    GapStart: int
    }

////////////////////////
/// Radix conversion ///
////////////////////////


// TODO: Rationalise by deleting these and using instead the functions in Helpers.

let private charList2String (charLst: char list) = 
    List.map string charLst
    |> List.toSeq
    |> String.concat ""

let dec2bin (n:bigint) (nBits:uint32) =
    [0u..nBits - 1u]
    |> List.rev
    |> List.map (fun bitNum -> if n &&& (1I <<< int bitNum) = 0I then '0' else '1')

let dec2hex (n: bigint) (nBits: uint32): string =
    let seqPad = 
        let times = (4 - int nBits % 4) % 4
        Seq.replicate times '0'

    let paddedBin =
        dec2bin n nBits
        |> Seq.append seqPad
        |> Seq.toList

    let fourBit2HexDig =
        let bit vec n = if (1 <<< n) &&& vec = 0 then '0' else '1'
        let hexDigOf n = (sprintf "%x" n)[0]
        [0..15]
        |> List.map (fun dig -> 
                List.map (bit dig) [3..-1..0], hexDigOf dig)
        |> Map.ofList

    [ 0 .. 4 .. List.length paddedBin - 4 ]
    |> List.map (fun i -> fourBit2HexDig[ paddedBin[i..i + 3] ])
    |> charList2String

let dec2sdec (n: bigint) (nBits: uint32) =
    if (dec2bin n nBits)[0] = '1' 
        then n - bigint (2.0 ** (float nBits)) 
        else n
    |> string

/// convert number to number string of the chosen radix
let n2StringOfRadix (hasRadixPrefix: bool) (n: bigint) (nBits: uint32) (rad: NumberBase) =
    let pref =
        match rad with
        | Bin -> "0b"
        | Hex -> "0x"
        | _ -> ""
    match rad with
    | Dec -> string n
    | Bin -> dec2bin n nBits |> charList2String
    | Hex -> dec2hex n nBits
    | SDec -> dec2sdec n nBits
    |> (fun numberRepStr -> (if hasRadixPrefix then pref else "") + numberRepStr)


/////////////////////////////
// General WaveSim Helpers //
/////////////////////////////

/// Get an option of the reduced canvas state, with geometry eliminated, good for electrical
/// circuit comparisons
let getReducedCanvState model = extractReducedState <| model.Sheet.GetCanvasState ()
(*    
/// get NetList from WaveSimModel
let wsModel2netList wsModel =
    match wsModel.LastCanvasState with
    | Some canvState -> Helpers.getNetList canvState
    | None -> Map.empty
*)
// Look up netgroup (= waveform) name from WaveData and netgroup
// If Netgroup is not in AllNetGroups return "ERROR"
let waveNameOf (ws:WaveSimModel) (wave: WaveformSpec) =
    Map.tryFindKey (fun _ p -> p = wave) ws.AllWaves
    |> Option.defaultValue "ERROR"

let reactTickBoxRow name nameStyle ticked toggleFun =   
    tr
        [ Class "rowHeight"
          Style [ VerticalAlign "middle" ] ]
        [ td
            [ Class "wAcheckboxCol"
              Class "rowHeight"
              Style [ VerticalAlign "middle" ] ]
              [ input
                  [ Type "checkbox"
                    Class "check"
                    Checked <| ticked
                    Style [ Float FloatOptions.Left ]
                    OnChange toggleFun ] ]
          td [] [ label [Style nameStyle] [ str name] ] ]           

////////////////////////



 
    
    


//-------------------------------------------------------------------------------------------------------//
//---------------------------------More wave Helpers-----------------------------------------------------//
//-------------------------------------------------------------------------------------------------------//

/// get components on other sheets, and RAMs, formatted for setting up "more" waveform selection.
/// The output is used as data for the "more wave select" popup.
let getWaveSetup (ws:WaveSimModel) (model:Model): MoreWaveSetup =
    /// get immediate subsheets of given sheet (sg,name,path)
    let rec subSheets ((sg,name,label,path):SimulationGraph*string*ComponentLabel*ComponentId list) : (SimulationGraph*string*ComponentLabel*ComponentId list) list =
        Map.toList sg
        |> List.collect (fun (cId,comp) -> 
            match comp.Type with
            | Custom custComp -> [Option.get comp.CustomSimulationGraph,  custComp.Name, comp.Label, path @ [cId]] | _ -> [])
    /// get all sheets rooted in (sg,name,path)
    let rec allSheets ((sg,name,label,path): SimulationGraph * string *ComponentLabel* ComponentId list) =
        match subSheets (sg,name,label,path) with
        | [] -> [sg,name,label,path]
        | sheets -> [sg,name,label,path] @ List.collect allSheets sheets
    let mainSheet = ((Option.get ws.InitWaveSimGraph).Graph, model.WaveSimSheet,ComponentLabel "", [])
    let sheets = allSheets mainSheet
    let getSortOf path (comp:SimulationComponent) = 
        match comp.Type, path with 
        | _,[] | _, [_] -> None
        | Input _,_-> Some (1,"Input")
        | Output _,_ -> Some (2, "Output")
        | IOLabel,_  -> Some (3, "Bus Label")
        | _ -> None
    let sheetCol =
        fun (sg, name, ComponentLabel label,path) ->
            Map.toList sg
            |> List.map (fun (cid,comp) -> getSortOf path comp, path @ [cid])
            |> List.sort
            |> List.collect (function | (Some (i,ctyp),path) -> [{Label = label;Sheet=name; Path=path;CSort=ctyp}] | _ -> [])
    sheets
    |> List.collect sheetCol
    |> (fun cols -> cols, (Set.ofList ws.SimParams.MoreWaves))

/// get component from graph subsheet with given name path
let rec getSimComp (sg:SimulationGraph) path =
    match path with
    | [] -> failwithf "What? Path cannot be [] looking up sim component in wave sim"
    | [cid]-> sg[cid]
    | h :: t -> 
        match sg[h].CustomSimulationGraph with
        | Some sg -> getSimComp sg t
        | None -> failwithf "What? A non-terminal part of a path must have a customSimulationgraph"

/// get component from graph subsheet with given name path
let rec getSimCompOpt (sg:SimulationGraph) path =
    match path with
    | [] -> None
    | [cid]-> Map.tryFind cid sg
    | h :: t -> 
        match Map.tryFind h sg with
        | Some {CustomSimulationGraph = Some sg} -> getSimCompOpt sg t
        | Some x -> failwithf "What? Lookup of compnent in simulationgraph failed"
        | None -> None

/// Get the form data for RAM (and other extra) simulation setup
let reactMoreWaves ((sheets,ticks): MoreWaveSetup) (sg:SimulationGraph) (dispatch: Msg -> Unit) =
    let makeTableCell r =   td [Style [VerticalAlign Top]] [r]
    let makeWaveReactList (swL:SheetWave list) =
        swL
        |> List.map (fun sw ->
            let comp = getSimComp sg sw.Path
            let ticked = Set.contains sw.Path ticks
            let toggle _ =
                let ticks = if ticked then Set.remove sw.Path ticks else Set.add sw.Path ticks
                dispatch <| SetPopupWaveSetup(sheets,ticks)
            let name = comp.Label |> function | ComponentLabel lab -> lab
            (sw.Label+":"+name),ticked, comp, toggle)
        |> (fun els -> 
                let isRamOrRom (comp: SimulationComponent) = 
                    match comp.Type with | _ -> false
                let uniques = 
                    List.countBy (fun (name,ticked,comp, toggle) -> name) els
                    |> List.filter (fun (el,i)-> i = 1)
                    |> List.map fst
                    |> Set
                List.filter (fun (name,ticked,comp, toggle) -> isRamOrRom comp && Set.contains name uniques) els)
        |> List.map (fun (name, ticked, comp, toggle) -> reactTickBoxRow name [] ticked toggle)
        
    let makeReactCol (name, sheetWaves) =
        let colBody = makeWaveReactList sheetWaves
        if colBody <> [] then 
            table [Style [Display DisplayOptions.InlineBlock; VerticalAlign Top; PaddingLeft "5px"; PaddingRight "5px"]] 
                    [tbody [Style [Display DisplayOptions.Inline]] <| [tr [] [th [ColSpan 2; Style [TextAlign TextAlignOptions.Center]] [str name]]] @ colBody]
        else div [] []
    
    let cols =
        sheets
        |> List.groupBy (fun {Sheet=name}->name)
        |> List.map makeReactCol
        |> List.map makeTableCell
    if cols = [] then
        str "There are no memories (RAM or ROM), in this design."
    else 
        table [] [tbody [] [tr [] cols]]

let formatMemory (mem:Memory1) =
    let maxFilledGap = 2L
    let sortedLocs = 
        Map.toList mem.Data
        |> List.sort
        |> List.filter (fun (a,d) -> d <> 0L)
    let makeRamLoc (addr: int64) (dat:int64) =
        let disp (v:int64) w = dec2hex (bigint v) (uint32 w)
        sprintf "[0x%s]: 0x%s" (disp addr mem.AddressWidth) (disp dat mem.WordWidth)
    let dispGap endG startG =
        match endG - startG with
        | x when x > maxFilledGap -> 
            [ "[...]: 0x0" ]
        | _ -> 
            [startG..endG - 1L]
            |> List.map (fun a -> makeRamLoc a 0L)

    let rec fillGaps curAddr lst =
        match lst with
        | [] -> dispGap ((1L <<< mem.AddressWidth) - 1L) curAddr
        | (addr, dat) :: lst' -> dispGap addr curAddr @ [ makeRamLoc addr dat ] @ fillGaps (addr+1L) lst'
    fillGaps 0L sortedLocs


/// return sample at current cursor position
let getCursorSampleFromGraph (wSMod: WaveSimModel) =
    let n = int wSMod.SimParams.CursorTime
    wSMod.SimDataCache[n].Graph

(*
/// get Ram contents as array to display. RAM contents is
/// determined on cursor sample from wSMod
let getRamInfoToDisplay wSMod (path: ComponentId list) =
    let cursorStep = wSMod.SimParams.CursorTime
    let sdOpt = Array.tryItem (int cursorStep) wSMod.SimDataCache
    let apLst = path |> List.rev |> function | (h::rest) -> h :: (List.rev rest) | [] -> []
    match sdOpt, apLst with
    | Some sd, (cid :: ap)  ->
        let state = FastRun.extractFastSimulationState sd.FastSim (int cursorStep) (cid,ap)
        let lab = match sd.FastSim.FComps[cid,ap].SimComponent.Label with |  ComponentLabel lab -> lab
        match state with
        | RamState dat  ->
            lab, formatMemory dat
        | _ -> lab, []
    | _ -> "",[]
   




//------------------------------------------------------------------------------------------------------//
//--------------------------------------NetGroup Helpers------------------------------------------------//
//------------------------------------------------------------------------------------------------------//





/// Get NetGroup from targets which represents the group of nLTargets connected by IOLabels.
/// targets:list of inputs connected to a single driving component output (e.g. a connected Net).
/// Return the containing NetGroup, where Nets connected by IOLabels form single Netgroups.
let rec private getNetGroup (netList: NetList) targets = failwithf "this function is no longer implemented"

/// returns a bool representing if the given NLTarget is present in the given NetList
let private isNetListTrgtInNetList (netList: NetList) (nlTrgt: NLTarget) =
    Map.exists (fun _ (nlComp: NetListComponent) -> 
                    Map.exists (fun _ nlTrgtLst -> List.contains nlTrgt nlTrgtLst) nlComp.Outputs) netList

                    (*
/// get array of TrgtLstGroup with the non-existing NLTargets removed
let private getReloadableNetGroups (model: Model) (netList: NetList) =
    match currWaveSimModel model with
    | Some wSModel ->
        dispWaves wSModel
        |> Array.map (fun netGroup -> netGroup.driverNet) 
        |> Array.map (List.filter <| isNetListTrgtInNetList netList)
        |> Array.filter ((<>) [])
        |> Array.map (getNetGroup netList)
    | None -> [||] *)
*)
(*
/// advance SimulationData by 1 clock cycle
let private clkAdvance (sD: SimulationData) =
    let sD =
        if sD.ClockTickNumber = 0 then
            // set up the initial fast simulation
            {sD with FastSim = match FastRun.buildFastSimulation (int maxLastClk) sD.Graph with | Ok fs -> fs | Error e -> failwithf "fast simulation error"}
        else
            sD
    //feedClockTick sD.Graph
    //|> (fun graph ->
    let newClock = sD.ClockTickNumber + 1
    FastRun.runFastSimulation newClock sD.FastSim
    { sD with
              Graph = sD.Graph
              ClockTickNumber = newClock }
*)

(*
/// array of SimData for the given number of cycles
let extractSimData simData nCycles =
    (simData, [| 1u .. nCycles |])
    ||> Array.mapFold (fun s _ -> 
         let s' = clkAdvance s
         (s',s'))
    |> fst

/// get NLSource option from ComponentId and InputPortNumber
let private drivingOutput (netList: NetList) compId inPortN =
    netList[compId].Inputs[inPortN]



/// get array of available NLSource in current canvas state
let availableNetGroups (model: Model) =
    match getSheetWaveSimOpt model with
    | None -> [||]
    | Some waveSim ->
        waveSim.LastCanvasState
        |> Option.defaultValue ([],[])
        |> Helpers.getNetList
        |> makeAllNetGroups



/// get instantaneous value of a port
let private simWireData2Wire wireData =
    wireData
    |> List.mapFold (fun weight bit ->
        match bit with
        | SimulatorTypes.Bit.Zero -> bigint 0
        | SimulatorTypes.Bit.One -> weight
        |> (fun r -> r, weight * (bigint 2))) (bigint 1)
    |> fst
    |> List.sum
*)   
(*
/// extract current value of the given array of SourceGroup
let getSimTime (waves: WaveformSpec array) (simData: SimulationData) =
    let fs = simData.FastSim
    let step = simData.ClockTickNumber
    let topLevelComps = simData.FastSim.FComps
    FastRun.runFastSimulation step fs
    waves
    |> Array.map (fun wave ->
        try 
            let driver,opn = wave.Driver
            let wD = FastRun.extractFastSimulationOutput fs step driver opn
            Wire
                { NBits = uint (List.length wD)
                  BitData = simWireData2Wire wD }
        with
        | e -> 
            printfn "Exception: %A" e.StackTrace
            printSimGraph simData.Graph
            failwithf "What? This error in getSimTime should not be possible"

        )
 *)
 (*
/// get all values of waveforms
let getAllWaveSimDataBySample (wsMod: WaveSimModel) =
        let waves = dispWaves wsMod
        wsMod.SimDataCache
        |> Array.map (getSimTime waves)

/// get values of waveforms for one sample
let getWaveSimDataOneSample (wsMod: WaveSimModel) (sample:int) =
    let waves = dispWaves wsMod
    wsMod.SimDataCache[sample]
    |> getSimTime waves
*)


