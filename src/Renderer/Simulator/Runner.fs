(*
    Runner.fs

    This module collects functions that allow to feed input into the simulation,
    effectively allowing to run it.
*)

module SimulationRunner

open CommonTypes
open Helpers
open SimulatorTypes
open SynchronousUtils

// During simulation, a Component Reducer function will produce the output only
// when all of the expected inputs have a value. Once this happens, it will
// calculate its outputs and set them in the next simulationComponent(s).

/// Function to determine what reducer inputs or outputs have changed.
let diffReducerInputsOrOutputs
        (newIO : Map<'a, WireData>)
        (oldIO : Map<'a, WireData>)
        : Map<'a, WireData> =
    // 'a type is either InputPortNumber or OutputPortNumber.
    // New inputs/outputs either:
    // - have more keys than old ones,
    // - have the same keys as old ones, but their values have changed.
#if ASSERTS
    assertThat (oldIO.Count <= newIO.Count) (sprintf "diffReducerInputsOrOutputs: (%A:%A)" oldIO newIO)
#endif
    (Map.empty, newIO)
    ||> Map.fold (fun diff portNumber wireData ->
        match oldIO.TryFind portNumber with
        | None -> diff.Add(portNumber, wireData)
        | Some oldData when oldData <> wireData -> diff.Add(portNumber, wireData)
        | Some oldData when oldData = wireData -> diff
        | _ -> failwith "what? Impossible case in diffReducerInputsOrOutputs"
    )

/// Feed zero to a simulation input.
/// This function is supposed to be used with Components of type Input.
let feedSimulationInput graph inputId wireData =
    failwithf "this function should never be called"

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes.
let getSimulationIOs
    (components : Component list)
    : SimulationIO list =
        ([], components) ||> List.fold (fun every comp ->
    match comp.Type with
    | _ -> ((ComponentId comp.Id, ComponentLabel comp.Label) :: every)
            )      

/// Get ComponentIds, ComponentLabels and wire widths of all input and output
/// nodes in a simulationGraph.
let getSimulationIOsFromGraph
    (graph : SimulationGraph)
    : SimulationIO list * SimulationIO list =
        (([], []), graph) ||> Map.fold (fun (inputs, outputs) compId comp ->
    match comp.Type with
    | Input w  -> ((comp.Id, comp.Label) :: inputs, outputs)
    | Output w -> (inputs, (comp.Id, comp.Label) :: outputs)
    | _ -> (inputs, outputs)
        )
