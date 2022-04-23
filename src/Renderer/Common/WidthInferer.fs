(*
Function to perform bus width inference on a canvas
*)

module BusWidthInferer

open CommonTypes
open Helpers

// 1. Initialise Map<ConnectionId, int option> for all connections, to None
//    (None means width not inferred yet).
// 2. Extract Input Components.
// 3. Starting from all those input components, run the inference process:
//    a. Case: component has all information about connections connected to it.
//       (for example, an input node or an and gate. They know they expect bits).
//       - Get the width of the incoming wires
//       - If there is any inconsistence, return the error
//       - Set the width of the outgoing wires
//       - follow the wires you just set and repeat the inference process on the
//         new components.
//    b. Case: component does not have all the information for computing the
//       width of outgoing wires yet.
//       (for example, a mergeBus components with only one bus connected)
//       - return

