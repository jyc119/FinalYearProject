(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open DiagramStyle
open ModelType
open CommonTypes
open PopupView
open Sheet.SheetInterface
open DrawModelType


let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

let private createComponent compType label model dispatch =
    Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, compType, label)) |> dispatch

// Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, 
// so give the label ""
let createCompStdLabel comp model dispatch =
    createComponent comp "" model dispatch



let private makeCustom styles model dispatch (loadedComponent: LoadedComponent)  =
    let canvas = loadedComponent.CanvasState
    menuItem styles loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = FilesIO.getOrderedCompLabels (Input 0) canvas
            OutputLabels = FilesIO.getOrderedCompLabels (Output 0) canvas
        }
        
        Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, custom, "")) |> dispatch
    )

let private makeCustomList styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.map (makeCustom styles model dispatch)

let private createIOPopup hasInt typeStr compType (model:Model) dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = model.LastUsedDialogWidth
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent (compType inputInt) (formatLabelFromType (compType inputInt) inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createResistorPopup hasInt typeStr compType (model:Model) dispatch =
    let title = sprintf "Add %s" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <|  "Resistance"
    let floatDefault = 0
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndFloat beforeText placeholder beforeInt floatDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let inputfloat = getFloat dialogData
            createComponent (compType inputfloat) (formatLabelFromType (compType inputfloat) inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch



/// two react text lines in red
let private twoErrorLines errMsg1 errMsg2 =
    span [Style [Color Red]] [str errMsg1; br []; str errMsg2; br [] ]

/// two line message giving constant value
let private constantValueMessage w (cVal:int64) =
    let mask = 
        if w = 64 then 
            0xffffffffffffffffUL 
        else 
            (1UL <<< w) - 1UL
    let uVal = (uint64 cVal) &&& mask
    let sVal = ((int64 uVal) <<< 64 - w) >>> 64 - w
    let hVal = NumberHelpers.fillHex64 w (int64 uVal)
    let line1 = $"Decimal value: %d{uVal} (%d{sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]

/// check constant parameters and return two react lines with
/// error message or value details
let parseConstant w cText =
    if w < 1 || w > 64 then
            twoErrorLines $"Constant width must be in the range 1..64" "", None
    else
        match NumberHelpers.strToIntCheckWidth w cText with
        | Ok n ->
            constantValueMessage w n, Some (Constant1 (w,n,cText))
        | Error msg ->
            twoErrorLines msg "", None

/// create react popup to set a constant
let private createConstantPopup model dispatch =
    let title = sprintf "Add Constant" 
    let beforeInt =
        fun _ -> str "How many bits has the wire carrying the constant?"
    let intDefault = 1
    let parseConstantDialog dialog =
        parseConstant 
            (Option.defaultValue intDefault dialog.Int)
            (Option.defaultValue "" dialog.Text)
    let beforeText = (fun d -> div [] [d |> parseConstantDialog |> fst; br [] ])
    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let width = getInt dialogData
            let text = Option.defaultValue "" dialogData.Text
            let constant = 
                match NumberHelpers.strToIntCheckWidth width text with
                | Ok n -> n
                | Error _ -> 0L // should never happen?
            let text' = if text = "" then "0" else text
            createCompStdLabel (Constant1(width,constant,text')) model dispatch
            dispatch ClosePopup
    let isDisabled = parseConstantDialog >> snd >> Option.isNone
    dialogPopup title body buttonText buttonAction isDisabled dispatch


    

let private createMemoryPopup memType model (dispatch: Msg -> Unit) =
    let title = "Create memory"
    let intDefault = model.LastUsedDialogWidth
    let addError errorOpt (memSetup:(int*int*InitMemData*string option) option) : (int*int*InitMemData*string option) option =
        match memSetup with
        | Some (n1,n2,mem, _) ->
            Some (n1,n2,mem,errorOpt)
        | _ -> None
    let body = dialogPopupBodyMemorySetup intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth, source, msgOpt = getMemorySetup dialogData intDefault
            let initMem = {
                AddressWidth = addressWidth
                WordWidth = wordWidth
                Init = source
                Data = Map.empty
                }

            let memory = FilesIO.initialiseMem  initMem dialogData.ProjectPath
            match memory with
            | Ok mem ->
                createCompStdLabel (memType mem)  model dispatch
                dispatch ClosePopup
            | Error mess ->
                dispatch <| SetPopupDialogMemorySetup (addError (Some mess) dialogData.MemorySetup)
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            let addressWidth, wordWidth, source,_ = getMemorySetup dialogData 1
            let error = 
                match dialogData.MemorySetup with 
                | Some(_,_,ToFileBadName _,_) ->
                    Some "File name must be alphanumeric without prefix"
                | None -> 
                    Some ""
                | Some (_,_,SignedMultiplier,_) 
                | Some(_,_,UnsignedMultiplier,_) ->
                    if addressWidth % 2 <> 0 then
                        Some "The address width must be even for a multiplier"
                    elif addressWidth > 16 then
                        Some "The maximum multiplier size is 8X8 - 16 address bits"
                    else None
                | _ -> 
                    None
            match error with
            | Some msg when msg <> "" ->
                match dialogData.MemorySetup with
                | Some (_,_,_,e) when e = Some msg -> ()
                | _ -> dispatch <| SetPopupDialogMemorySetup (addError (Some msg) dialogData.MemorySetup)
            | _ -> ()
            addressWidth < 1 || wordWidth < 1 || error <> None
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private makeMenuGroup title menuList =
    details [Open false] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]


let mutable firstTip = true

let mutable tippyNodes: Browser.Types.Element list = []

let private makeMenuGroupWithTip styles  title tip menuList =
    details [
        Open false;
        HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
        Tooltip.dataTooltip tip
        Style styles
    ] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let compareModelsApprox (m1:Model) (m2:Model) =

    let m1r = reduceApprox m1
    let m2r = reduceApprox m2
    let b = m1r = m2r
    //printfn "Model equality:%A" b
    //if b = false then printfn "\n\n%A\n\n%A\n\n" m1r m2r
    b

let viewCatalogue model dispatch =
        let viewCatOfModel = fun model ->                 
            let styles = 
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []

            let catTip1 name func (tip:string) = 
                let react = menuItem styles name func
                div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                      Tooltip.dataTooltip tip
                      Style styles
                    ]
                    [ react ]
            Menu.menu [Props [Class "py-1"; Style styles]]  [
                // TODO
                    makeMenuGroup
                        "Input / Output"
                        [ catTip1 "Input"  (fun _ -> createIOPopup true "input" Input model dispatch) "Input connection to current sheet: one or more bits"
                          catTip1 "Output" (fun _ -> createIOPopup true "output" Output model dispatch) "Output connection from current sheet: one or more bits"]
                    makeMenuGroup
                        "Linear Components"
                        [ catTip1 "Resistor"  (fun _ -> createResistorPopup true "Resistor" Resistor model dispatch) "Resistor"
                          catTip1 "Voltage Source"  (fun _ -> createCompStdLabel VoltageSource model dispatch) "Voltage Source"]
                    //makeMenuGroup
                        //"Non-Linear components"
                        //[ catTip1 "CurrentSource"  (fun _ -> createCompStdLabel CurrentSource model dispatch) "Current ource"]
                    makeMenuGroupWithTip styles
                        "This project"
                        "Every design sheet is available for use in other sheets as a custom component: \
                        it can be added any number of times, each instance replicating the sheet logic"
                        (makeCustomList styles model dispatch)
                ]

        (viewCatOfModel) model 
