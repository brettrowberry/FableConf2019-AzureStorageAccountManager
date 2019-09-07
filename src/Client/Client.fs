module Client

open Elmish
open Elmish.React
open Fable.React
open Fulma

open Shared
open Fulma.Extensions.Wikiki

type Model = {
   Accounts : FableStorageAccount []
   IsListing : bool
   Nickname: string option
   IsCreating: bool
   IdsToDelete : Set<string>
   IsDeleting : bool
   Error : exn option
}

type Msg =
| List
| ListOk of FableStorageAccount []
| ListErr of exn

| Create of string
| CreateOk of unit
| CreateErr of exn

| Delete of string[]
| DeleteOk of unit
| DeleteErr of exn

| NicknameBox of string
| ToggleSelect of string

| RemoveError

module Server =
    open Fable.Remoting.Client

    let api : IStorageApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IStorageApi>

let listCmd =
    Cmd.OfAsync.either
        Server.api.list
        ()
        ListOk
        ListErr

let createCmd name  =
    Cmd.OfAsync.either
        Server.api.create
        name
        CreateOk
        CreateErr

let deleteCmd ids =
    Cmd.OfAsync.either
        Server.api.delete
        ids
        DeleteOk
        DeleteErr

let init () : Model * Cmd<Msg> =
    let initialModel = {
        Accounts = Array.empty
        IsListing = false
        Nickname = None
        IsCreating = false
        IdsToDelete = Set.empty
        IsDeleting = false
        Error = None
      }
    initialModel, listCmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | List -> {model with IsListing = true }, listCmd
    | ListOk accounts -> { model with IsListing = false; Accounts = accounts }, Cmd.none
    | ListErr exn -> { model with IsListing = false; Error = Some exn }, Cmd.none

    | Create name -> { model with IsCreating = true }, createCmd name
    | CreateOk _ -> { model with Nickname = None; IsCreating = false }, listCmd
    | CreateErr exn -> { model with Nickname = None; IsCreating = false; Error = Some exn }, Cmd.none

    | Delete ids -> { model with IsDeleting = true }, deleteCmd ids
    | DeleteOk _ ->
        let newModel = { model with IsDeleting = false; IdsToDelete = Set.empty }
        newModel, listCmd
    | DeleteErr exn -> { model with IsDeleting = false; Error = Some exn }, Cmd.none

    | ToggleSelect id ->
        let newSet =
          if model.IdsToDelete.Contains id
          then model.IdsToDelete.Remove id
          else model.IdsToDelete.Add id
        { model with IdsToDelete = newSet }, Cmd.none

    | NicknameBox str ->
        let nickname =
            if str.Length < 1 then None
            else Some str
        { model with Nickname = nickname }, Cmd.none

    | RemoveError -> { model with Error = None }, Cmd.none

let viewCreate model (dispatch : Msg -> unit) =
    Container.container [] [
        Label.label [] [ str "Storage Account Nickname" ]
        Input.text [Input.OnChange(fun f -> NicknameBox f.Value |> dispatch); Input.Value (defaultArg model.Nickname "")]
        Button.a [
            Button.OnClick(fun _ -> (Create model.Nickname.Value) |> dispatch);
            Button.Disabled (model.Nickname.IsNone || model.IsCreating)]
          [ str "Create" ]
    ]

let deleteButton ids dispatch =
    Button.a [Button.OnClick(fun _ -> Delete ids |> dispatch); Button.Disabled (ids.Length <= 0) ] [ str "Delete"]

let viewCommands model (dispatch : Msg -> unit) =
    let ids = model.IdsToDelete |> Set.toArray

    Container.container [] [
        Divider.divider [ ]
        deleteButton ids dispatch
        viewCreate model dispatch
        ]

let viewAccountRow isSelected (sa : FableStorageAccount) dispatch =
  tr [] [
    td [] [ Checkradio.checkbox [
      Checkradio.Id sa.Name
      Checkradio.Checked isSelected
      Checkradio.OnChange (fun _ -> ToggleSelect sa.Id |> dispatch) ] []]
    td [] [ str (String.concat ", " (sa.Tags |> Array.map snd)) ]
    td [] [ str sa.Name ]
    td [] [ str sa.Region ]
    ]

let viewAccounts (model : Model ) (dispatch : Msg -> unit) =
  let tableBody =
    [ for x in model.Accounts ->
      viewAccountRow (model.IdsToDelete.Contains x.Id) x dispatch]
  Table.table [ Table.IsBordered; Table.IsStriped; Table.IsHoverable; Table.IsFullWidth ] [
    thead [] [
      tr [] [
        th [] []
        th [] [ str "Nickname" ]
        th [] [ str "Name" ]
        th [] [ str "Region" ]
      ]
    ]
    tbody []
      tableBody
  ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        (seq {
          yield viewAccounts model dispatch
          yield viewCommands model dispatch
           })


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
