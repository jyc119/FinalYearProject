open System
open System.IO
open Extreme.Mathematics
open Extreme.Mathematics.LinearAlgebra
open Plotly.NET
open ACHelper

/// Calculates the magnitude of a complex number
let magnitudeOfComplex (complex:Complex<float>) = 
    let magnitude = complex.Magnitude
    magnitude

/// Calculates the phase of a complex number
let phaseOfComplex (complex:Complex<float>) = 
    let radian = complex.Phase
    radian * (180.0/Math.PI)

/// Creates a complex number from circuit components
let createComplex capacitance resistance (current:float) frequency = 
    let capacitorImpedance = 2.0*Math.PI*frequency*capacitance
    let totalImpedance = Complex<float>(1.0/resistance,capacitorImpedance)
    current * 1.0/totalImpedance

/// Creates bode plot from magnitude and phase list
let plotBodePlot (magLst:float list) (phaseLst: float list) =
    let x = List.append [0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9] [1.0..100000.0]

    let logMag = 
        magLst
        |> List.map(fun sp -> 20.0 * Math.Log10 sp)

    let phasePlot = 
        Chart.Line(x, phaseLst,Name="Phase")
        |> Chart.withAxisAnchor(Y=2)
        |> Chart.withXAxisStyle("Frequency",AxisType = StyleParam.AxisType.Log)

    let magPlot = 
        Chart.Line(x,logMag,Name="Magnitude")
        |> Chart.withAxisAnchor(Y=1)
        |> Chart.withXAxisStyle("Frequency",AxisType = StyleParam.AxisType.Log)
        
    let twoAxes = 
        [
            magPlot
            phasePlot
         ]
         |> Chart.combine
         |> Chart.withYAxisStyle(
             "decibels(dB)",
             Side=StyleParam.Side.Left,
             Id=StyleParam.SubPlotId.YAxis 1
         )
         |> Chart.withYAxisStyle(
             "degree",
             Side=StyleParam.Side.Right,
             Id=StyleParam.SubPlotId.YAxis 2,
             Overlaying=StyleParam.LinearAxisId.Y 1
         )

    twoAxes
    |> Chart.show
    
/// Simulates an RC filter
let simulateRCFilter (data:RCFilter) = 
    let resistor = data.Resistor
    let capacitor = data.Capacitance
    let voltage = data.Voltage
    let current = voltage/resistor
    let frequency = List.append [0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9] [1.0..100000.0]

    let complexNumberList = 
        frequency
        |> List.map (createComplex capacitor resistor current)

    let magnitudeList = 
        complexNumberList
        |> List.map magnitudeOfComplex

    let phaseList = 
        complexNumberList
        |> List.map phaseOfComplex

    plotBodePlot magnitudeList phaseList