module FastReduce
open CommonTypes
open SimulatorTypes
open NumberHelpers
(*
//------------------------------------------------------------------------------//
//-----------------------------Fast Reduction of Components---------------------//
//------------------------------------------------------------------------------//



//------------------------------------------------------------------------------//
// ----------Subfunctions used by fastReduce to evaluate a component-----------//
//------------------------------------------------------------------------------//

let inline assertThat cond msg = 
    if not cond
    then failwithf "what? assert failed: %s" msg

/// Assert that the FData only contain a single bit, and return such bit.
let inline extractBit (fd: FData) (busWidth: int) : uint32 =
#if ASSERTS
    assertThat (fd.Width = 1 || fd.Width=2 || fd.Width=3)
    <| sprintf "extractBit called with wireData: %A" fd
#endif
    match fd.Dat with | Word n -> n | BigWord _ -> failwithf $"Can't extract %d{busWidth} bit from BigWord data {fd.Dat} of width {fd.Width}"

let inline packBit (bit: uint32) : FData = if bit = 0u then {Dat=Word 0u; Width = 1} else {Dat = Word 1u; Width = 1}


/// Read the content of the memory at the specified address.
let private readMemory (mem: Memory1) (address: FData) : FData =
    let intAddr = convertFastDataToInt64 address
    let outDataInt = Helpers.getMemData (int64 intAddr) mem
    convertInt64ToFastData mem.WordWidth outDataInt

/// Write the content of the memory at the specified address.
let private writeMemory (mem: Memory1) (address: FastData) (data: FastData) : Memory1 =
    let intAddr = int64 <| convertFastDataToInt64 address
    let intData = int64 <| convertFastDataToInt64 data

    { mem with
          Data = Map.add intAddr intData mem.Data }

let private getRamStateMemory numSteps step (state: StepArray<SimulationComponentState> option) memory : Memory1 =
    match state, numSteps with
    | _, 1 -> memory
    | Some arr, _ ->
        match arr.Step[step] with
        | RamState memory -> memory
        | _ -> failwithf "What? getRamStateMemory called with invalid state"
    | _ -> failwithf "what? getRamStateMemory called with an invalid state: %A" state

let getRomStateMemory comp =
    match comp.FType with
    | _ -> failwithf "What? getRomStateMemory called with invalid state"


let inline private bitNot bit = bit ^^^ 1u

let inline private bitAnd bit0 bit1 = bit0 &&& bit1


let inline private bitOr bit0 bit1 = bit0 ||| bit1


let inline private bitXor bit0 bit1 = bit0 ^^^ bit1


let inline private bitNand bit0 bit1 = bitAnd bit0 bit1 |> bitNot

let inline private bitNor bit0 bit1 = bitOr bit0 bit1 |> bitNot

let inline private bitXnorXnor bit0 bit1 = bitXor bit0 bit1 |> bitNot




//---------------------------------------------------------------------------------------//
// ------------------------------MAIN COMPONENT SIMULATION FUNCTION----------------------//
//---------------------------------------------------------------------------------------//


/// Given a component, compute its outputs from its inputs, which must already be evaluated.
/// Outputs and inputs are both contained as time sequences in arrays. This function will calculate
/// simStep outputs from (previously calculated) simStep outputs and clocked (simStep-1) outputs.
/// Memory has state separate from simStep-1 output, for this the state is recalculated.
let fastReduce (maxArraySize: int) (numStep: int) (isClockedReduction: bool) (comp: FastComponent) : Unit =
    let componentType = comp.FType

    //printfn "Reducing %A...%A %A (step %d) clocked=%A"  comp.FType comp.ShortId comp.FullName numStep isClockedReduction
    let n = comp.InputLinks.Length

    let simStep = numStep % maxArraySize 
    let simStepOld = if simStep = 0 then maxArraySize - 1 else simStep - 1


    ///  get data feom input i of component
    let inline ins i =
#if ASSERTS
        assertThat (i < n) (sprintf "What? Invalid input port (%d:step%d) used by %s:%s (%A) reducer with %d Ins"
                                    i simStep comp.FullName comp.ShortId  componentType n)
#endif
        let fd = comp.InputLinks[i].Step[simStep]
        fd

    /// get last cycle data from output i (for clocked components)
    let inline getLastCycleOut n =
        let fd =
            match comp.OutputWidth[n], numStep with
            | None, _ -> failwithf "Can't reduce %A (%A) because outputwidth is not known" comp.FullName comp.FType
            | Some w, 0 -> if w < 33 then {Dat=Word 0u; Width = w} else {Dat =BigWord (bigint 0); Width = w}
            | Some w, _ -> comp.Outputs[n].Step[simStepOld]
        fd

    /// get last cycle data from output i for component
    let inline insOld i = 
#if ASSERTS
        assertThat
            (i < n)
            (sprintf
                "What? Invalid input port (%d:step%d) used by %s (%A) reducer with %d Ins"
                i
                simStep
                comp.FullName
                componentType
                n)
#endif

        let fd =
            comp.GetInput(simStepOld) (InputPortNumber i) 
        fd

    /// Write current step output data for output port 0
    let inline put0 fd =
        comp.PutOutput(simStep) (OutputPortNumber 0) fd

    /// Write current step output data for output port 1
    let inline put1 fd =
        comp.PutOutput(simStep) (OutputPortNumber 1) fd

    /// Write current step output data for output port 2
    let inline put2 fd =
        comp.PutOutput(simStep) (OutputPortNumber 2) fd
    
    /// Write current step output data for output port 3
    let inline put3 fd =
        comp.PutOutput(simStep) (OutputPortNumber 3) fd
    
    /// Write current step output data for output port 4
    let inline put4 fd =
        comp.PutOutput(simStep) (OutputPortNumber 4) fd
    
    /// Write current step output data for output port 4
    let inline put5 fd =
        comp.PutOutput(simStep) (OutputPortNumber 5) fd
    
    /// Write current step output data for output port 4
    let inline put6 fd =
        comp.PutOutput(simStep) (OutputPortNumber 6) fd

    /// Write current step output data for output port 4
    let inline put7 fd =
        comp.PutOutput(simStep) (OutputPortNumber 7) fd

    /// Write current State (used only for RAMs, DFFs and registers use previous cycle output as state)
    let inline putState state =
        match comp.State with
        | None -> failwithf "Attempt to put state into component %s without state array" comp.FullName
        | Some stateArr -> stateArr.Step[simStep] <- state

    let inline putW num w = comp.OutputWidth[num] <- Some w

    /// implement a binary combinational operation
    let inline getBinaryGateReducer (op: uint32 ->uint32 -> uint32) : Unit =
        let bit0 = (ins 0).GetQUint32
        let bit1 = (ins 1).GetQUint32
        put0 <| {Width=1; Dat = Word (op bit1 bit0)}

    /// Error checking (not required in production code) check widths are consistent
    let inline checkWidth width (bits: FData) = 
#if ASSERTS
        assertThat
            (bits.Width = width)
            (sprintf
                "Input node reducer for (%A:%A - STEP %d) received wrong number of bits: expected %d but got %d"
                comp.FullName
                comp.FType
                simStep
                width
                bits.Width)
#else
        ()
#endif

    // reduce the component in this match
    match componentType with
    | Input width ->
        if comp.Active then
            let bits = ins 0
            //printfn "Got input 0 = %A Links=<%A> len=%d" bits comp.InputLinks comp.InputLinks.Length
            checkWidth width bits
            //printfn "output array = %A" comp.Outputs
            put0 bits
    //printfn "Finished!"

    | Constant1 (width, cVal,_) ->
        put0
        <| convertInt64ToFastData width cVal
    | Output width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put0 bits
    | Viewer width ->
        let bits = ins 0
        //printfn "In output bits=%A, ins = %A" bits comp.InputLinks
        checkWidth width bits
        put0 bits

    | IOLabel ->
        let bits = ins 0
        //let bits = comp.InputLinks[0][simStep]
        //printfn "Reducing IOLabel %A" comp.SimComponent.Label
        put0 bits
    | Custom c ->
        // Custom components are removed
        failwithf "what? Custom components are removed before the fast simulation: %A" c
    | _ ->
        let bits = ins 0
        //let bits = comp.InputLinks[0][simStep]
        //printfn "Reducing IOLabel %A" comp.SimComponent.Label
        put0 bits
*)


