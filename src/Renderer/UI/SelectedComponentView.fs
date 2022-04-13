(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open ModelType
open CommonTypes
open MemoryEditorView
open PopupView
open Notifications
open Sheet.SheetInterface
open DrawModelType

let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]

let private textFormField isRequired name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]

let private textFormFieldSimple name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]


let private intFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private int64FormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getInt64EventValue >> onChange)
        ]
    ]

let private intFormFieldNoMin name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width "60px"]]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private int64FormFieldNoMin name (defaultValue:int64) (currentText:string option) onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [Style [Width "180px"]]
            Input.DefaultValue <| Option.defaultValue $"{defaultValue}" currentText
            Input.OnChange (getTextEventValue >> onChange)
        ]
    ]


let getInitSource (mem: Memory1) =
    let a = mem.AddressWidth
    match mem.Init with
    | SignedMultiplier -> 
        $"Dout = (signed) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | UnsignedMultiplier ->
        $"Dout = (unsigned) addr({a-1}:{a/2}) * addr({a/2-1}:0)"
    | FromData ->
        "See Memory Viewer"
    | FromFile name | ToFile name | ToFileBadName name ->
        $"From '{name}.ram' file"
   

let private makeMemoryInfo descr mem compId cType model dispatch =
    let projectPath = (Option.get model.CurrentProj).ProjectPath
    div [] [
        str descr
        br []; br []
        str <| sprintf "Address width: %d bit(s)" mem.AddressWidth
        br []
        str <| sprintf "Number of elements: %d" (1UL <<< mem.AddressWidth)
        br []
        str <| sprintf "Word width: %d bit(s)" mem.WordWidth
        br []
        str <| sprintf "%sData: %s"  
                (match cType with 
                 | _ -> failwithf $"What - wrong component type ({cType} here")
                (getInitSource mem)
        br []
        //makeSourceMenu model (Option.get model.CurrentProj) mem dispatch
        br []; br []
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> openMemoryEditor mem compId model dispatch)
        ] [str "View/Edit memory content"]
        br []; br [];
        Button.button [
            Button.Color IsPrimary
            Button.OnClick (fun _ -> 
                FilesIO.openWriteDialogAndWriteMemory mem projectPath
                |> (function
                        | None -> ()
                        | Some path ->
                            let note = successPropertiesNotification $"Memory content written to '{path}'"
                            dispatch <| SetPropertiesNotification note)
                )
        ] [str "Write content to file"]

    ]

let private makeNumberOfBitsField model (comp:Component) text dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let title, width =
        match comp.Type with
        | Input w | Output w | Viewer w -> "Number of bits", w
        | Constant1(w, _,_) -> "Number of bits in the wire", w
        | c -> failwithf "makeNumberOfBitsField called with invalid component: %A" c
    intFormField title "60px" width 1 (
        fun newWidth ->
            if newWidth < 1
            then
                let props = errorPropsNotification "Invalid number of bits."
                dispatch <| SetPropertiesNotification props
            else
                model.Sheet.ChangeWidth sheetDispatch (ComponentId comp.Id) newWidth
                let text' = match comp.Type with | _ -> formatLabelAsBus newWidth text
                //SetComponentLabelFromText model comp text' // change the JS component label
                let lastUsedWidth = 
                    match comp.Type with 
                    | Constant1 _ -> 
                        model.LastUsedDialogWidth 
                    | _ ->  
                        newWidth
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch <| SetPopupDialogInt (Some newWidth)
                dispatch ClosePropertiesNotification
    )





let mockDispatchS msgFun msg =
    match msg with
    | Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol sMsg)) ->
        msgFun msg
    | _ -> ()



let msgToS = 
    BusWireT.Msg.Symbol >> SheetT.Msg.Wire >> Msg.Sheet
  
/// Return dialog fileds used by constant, or default values
let constantDialogWithDefault (w,cText) dialog =
    let w = Option.defaultValue w dialog.Int
    let cText = Option.defaultValue cText dialog.Text
    w, cText

/// Create react to chnage constant properties
let makeConstantDialog (model:Model) (comp: Component) (text:string) (dispatch: Msg -> Unit): ReactElement =
        let symbolDispatch msg = dispatch <| msgToS msg
        let wComp, txtComp =
            match comp.Type with | Constant1( w,_,txt) -> w,txt | _ -> failwithf "What? impossible" 
        let w = Option.defaultValue wComp model.PopupDialogData.Int
        let cText = Option.defaultValue txtComp model.PopupDialogData.Text
        let reactMsg, compTOpt = CatalogueView.parseConstant w cText
        match compTOpt with
        | None -> ()
        | Some (Constant1(w,cVal,cText) as compT) ->
            if compT <> comp.Type then
                model.Sheet.ChangeWidth (Sheet >> dispatch) (ComponentId comp.Id) w
                symbolDispatch <| SymbolT.ChangeConstant (ComponentId comp.Id, cVal, cText)
                dispatch (ReloadSelectedComponent w)
                dispatch ClosePropertiesNotification
        | _ -> failwithf "What? impossible"

        div [] [
                makeNumberOfBitsField model comp text dispatch
                br []
                reactMsg
                br []
                textFormFieldSimple 
                    "Enter constant value in decimal, hex, or binary:" 
                    cText 
                    (fun txt -> 
                        printfn $"Setting {txt}"
                        dispatch <| SetPopupDialogText (Some txt))
                
            ]              

let private makeLsbBitNumberField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let lsbPos, infoText =
        match comp.Type with 
        | _ -> failwithf "makeLsbBitNumberfield called from %A" comp.Type

    match comp.Type with
    | _ -> failwithf "What? invalid component for lsbpos in properties"



let private makeDescription (comp:Component) model dispatch =
    match comp.Type with
    | Input _ -> str "Input."
    | Constant1 _ -> str "Constant Wire."
    | Output _ -> str "Output."
    | Viewer _ -> str "Viewer."
    | IOLabel -> div [] [
        str "Label on Wire or Bus. Labels with the same name connect wires. Each label has input on left and output on right. \
            No output connection is required from a set of labels. Since a set represents one wire of bus, exactly one input connection is required. \
            Labels can be used:"  
        br [] ;
        str "To name wires and document designs."; br []
        str "To join inputs and outputs without wires."; br []
        str "To prevent an unused output from giving an error."
        ]
    | Resistor -> div [] [ str "Resistor" ]
    | CurrentSource -> div [] [ str "Current Source" ]
    | Custom custom ->
        let styledSpan styles txt = span [Style styles] [str <| txt]
        let boldSpan txt = styledSpan [FontWeight "bold"] txt
        let italicSpan txt = styledSpan [FontStyle "italic"] txt

        let toHTMLList =
            List.map (fun (label, width) -> li [] [str <| sprintf "%s: %d bit(s)" label width])
        div [] [
            boldSpan $"{custom.Name}"
            span [] [str <| ": user defined (custom) component."]
            br []
            br []
            p [  Style [ FontStyle "italic"; FontSize "12px"; LineHeight "1.1"]] [
                str <| $"Input or Output ports are displayed on the '{custom.Name}' symbol sorted by the \
                    vertical position on the design sheet of the Input or Output components at the time the symbol is added."]
            
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Inputs"]
            ul [] (toHTMLList custom.InputLabels)
            br []
            span [Style [FontWeight "bold"; FontSize "15px"]] [str <| "Outputs"]
            ul [] (toHTMLList custom.OutputLabels)
        ]

let private makeExtraInfo model (comp:Component) text dispatch =
    match comp.Type with
    | Input _ | Output _ | Viewer _ ->
        makeNumberOfBitsField model comp text dispatch
    | Constant1 _ ->         
             makeConstantDialog model comp text dispatch
    | _ -> div [] []


let viewSelectedComponent (model: ModelType.Model) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    let formatLabelText (txt: string) =
        txt.ToUpper()
        |> Seq.filter (function | ch when System.Char.IsLetterOrDigit ch -> true | '.' -> true | '_' -> true | _ -> false)
        |> Seq.skipWhile (System.Char.IsLetter >> not)
        |> (fun chars -> match Seq.length chars with | 0 -> None | _ -> Some (String.concat "" (Seq.map string chars)))
    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [
            // let label' = extractLabelBase comp.Label
            // TODO: normalise labels so they only contain allowed chars all uppercase
            let label' = Option.defaultValue "L" (formatLabelText comp.Label) // No formatting atm
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp label' dispatch
            let required = match comp.Type with  | _ -> true
            textFormField required "Component Name" label' (fun text ->
                // TODO: removed formatLabel for now
                //setComponentLabel model sheetDispatch comp (formatLabel comp text)
                match formatLabelText text with
                | Some label -> 
                    setComponentLabel model sheetDispatch comp label
                    dispatch <| SetPopupDialogText (Some label)
                | None -> ()
                //updateNames model (fun _ _ -> model.WaveSim.Ports) |> StartWaveSim |> dispatch
                dispatch (ReloadSelectedComponent model.LastUsedDialogWidth) // reload the new component
                )
        ]    
    | _ -> div [] [ str "Select a component in the diagram to view or change its properties, for example number of bits." ]

