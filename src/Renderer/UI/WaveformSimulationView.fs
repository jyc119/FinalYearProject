//(*
//WaveformSimulationView.fs
//
//View for waveform simulator in tab
//*)
//
module rec WaveformSimulationView
//
//(*******************************************************************************************
//
//Waveform simulation simulates the current sheet circuit and generates waveforms as react SVG elements
//on an SVG canvas. The waveform display changes interactively based on zoom and cursor movement buttons.
//
//In addition the SVG canvas is an element wider then the pane in which it is displayed, and uses HTML scrolling
//navigate. Zooming will recreate a new (wider) canvas as necessary so that the entire visible window contains valid
//waveforms.
//
//In addition the canvas consists only of those waveforms currently selected to be displayed, via a wave editor pane
//which displayed the names of all possible waveforms and allows them to be displayed or not. See waveeditor below
//for details.
//
//When the selected waveforms are changed the SVG canvas must also be recreated with new waveforms, although the
//simulation need not be advanced.
//
//Waveform simulation is initiated by the Waveforms >> button. This button is recreated by mainview a suitable color:
//green: a changed (or initial) circuit exists which can be (re-)simulated with waveform simulator
//orange: errors exist in circuit - no change to waveform simulation.
//white: circuit has not chnaged and is still the same as what is currently displaying waveforms
//
//The waveforms last simulated are thus displayed while the circuit can be updated, until the button is clicked. On
//this click, if the circuit has errors the old waveforms are replaced by an error message, otherwise a new simulation
//replaces the old simulation.
//
//Allowing simultaneous view (and interactive change as above) of old waveforms while letting the design be changed
//requires careful state management:
//
//model.LastSimulatedCanvasState - circuit last simulated
//model.Simulation - time = 0 SimGraph or simulation error for LastSimulatedCanvasState. A simulation can be extended by advancing the 
//SimGraph one or more simulation steps.
//
//Wavesim specific parameters and temporary state are inside a WaveSimModel record accessed via a Map field in the model:
//
//WaveSim: Map<string,waveSimModel>
//accessed as:
//model.WaveSim[openSheetName]: WaveSimModel
//
//TODO: The WaveSimModel is only valid when a project is open and the Wavesim data in reality should be part of
//the current sheet LoadedComponent record, tus WaveSim record moves inside LoadedComponent record and no longer
//needs to be a Map.
//
//The WaveSimModel data is changed on opening a different sheet or saving / restoring a project in exactly the same way as the
//rest of the sheet data LoadedComponentData. The simulation parameters last used for each sheet are saved and restored. Inside
//WaveSim a subrecord WaveSim.WaveData contains transient information about the current simulation which is not saved across sheet
//changes. In addition the field SimDataCache is transient and replaced whenever LastSimulatedCanvasState changes.
//
//TODO: make SimDataCache a field of WaveData. Move WaveData from WaveSimModel to Model - so there is just one copy of it, not
//one per sheet.
//
//waveform Simulation State Changes
//
//(1) SimulateButtonFunc definition:
//waveforms >> button pressed with SimIsStale = true, makeSimData model returns (Ok simData), Canvas exists and contains canvasData
//
//--> startNewWaveSimulation: this updates model with 
//        SimIsStale = false, 
//        WaveSim data for current sheet (waveSim) all initialised, 
//        waveSim.WaveData initialised from simData and canvasData
//        LastSimulatedCanvasState updated to equal the new circuit (?)
//
//*******************************************************************************************)
//
open Fulma
open Fable.React
open Fable.React.Props


open ModelType
open DiagramStyle
open CommonTypes
open WaveSimHelpers
open FileMenuView
open SimulatorTypes
open Sheet.SheetInterface
open DrawModelType



