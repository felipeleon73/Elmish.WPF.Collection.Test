module SingleCounter.Program

open System

type Item = 
  { Id: Guid
    Name: string }

type Model =
  { Count: int
    Items: Item list 
    Selected: Item option}

let data = 
    [1..2]
    |> List.map (fun i -> { Id=Guid.NewGuid(); Name=sprintf "Item %i" i }) 

let init () =
  { Count = data.Length
    Items = data 
    Selected = None}

type Msg =
  | Update of Guid*string
  | Select of Item option

let setNewVal id (value: string) items=
    let updateIf item =
        if item.Id = id then { item with Name = value.ToUpper() } else item
    items |> List.map updateIf

let update msg m =
  match msg with
  | Update (id,value) -> { m with Items = m.Items |> setNewVal id value }
  | Select value -> { m with Selected=value }

open Elmish.WPF
open Elmish.WPF.Internal

let itemBindings () : Internal.BindingSpec<Model*Item,Msg> list = 
  [
    "Value" |> Binding.twoWay 
        (fun m -> (snd m).Name)
        (fun v m -> Update ((snd m).Id,v) )
  ]

let bindings model dispatch =
  [
    "Count" |> Binding.oneWay (fun m -> m.Count)
    "Selected" |>  Binding.twoWay 
                    (fun m -> unbox m.Selected) //<-----------here is the issue! m.Selected is a Model, expect a ViewModel
                    (fun (v:ViewModel<_,_>) m -> Update v.CurrentModel)
    "Items" |> Binding.subBindingSeq
        id
        (fun m -> m.Items)
        (fun c -> c.Id)
        itemBindings
  ]

open System
open Elmish
open Elmish

[<EntryPoint; STAThread>]
let main argv =
  Program.mkSimple init update bindings
  |> Program.withConsoleTrace 
  |> Program.runWindow (MainWindow())