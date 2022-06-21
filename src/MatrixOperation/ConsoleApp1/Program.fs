open System

// For more information see https://aka.ms/fsharp-console-apps
(*
let path = "D:\HLP\Testing\myTest.txt"
let main =
    let x = ["ooga";"booga"]
    //let y = List.map System.Math.Sqrt x
     
    let xprecision = 3
    let yprecision = 5
     
    use file = System.IO.File.CreateText(path)
    let line = sprintf "%s"
    List.iter (fun x -> file.WriteLine (line x)) x

*)

let x = 1
let add2 = x+2

printfn "Addition: %A"(add2)