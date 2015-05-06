#I "packages/FSharp.Compiler.Service.0.0.73/lib/net45/"
#r "FSharp.Compiler.Service.dll"
#load "load-wpf.fsx"

open System
open System.IO
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open System.Windows
open System.Windows.Controls
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open FsiWpf
open Microsoft.FSharp.Compiler

let checker = FSharpChecker.Create()

let exampleProject = @"D:\Appdev\FSharp.Compiler.Service.HandsOn\example\example.fsproj"

let projectOptions = checker.GetProjectOptionsFromProjectFile(exampleProject) 

type MyFileSystem() = 
    let dflt = Shim.FileSystem
    
    let files = Dictionary<string,(DateTime * string)>()

    member __.SetFile(file, text:string) = 
         files.[file] <- (DateTime.Now, text)

    interface IFileSystem with

        member __.FileStreamReadShim(fileName) = 
            if files.ContainsKey(fileName) then
                let (fileWriteTime, fileText) = files.[fileName]
                new MemoryStream(Encoding.UTF8.GetBytes(fileText)) :> Stream
            else 
                dflt.FileStreamReadShim(fileName)

        member __.ReadAllBytesShim(fileName) =
            if files.ContainsKey(fileName) then
                let (fileWriteTime, fileText) = files.[fileName]
                Encoding.UTF8.GetBytes(fileText)
            else
                dflt.ReadAllBytesShim(fileName)

        member __.GetLastWriteTimeShim(fileName) = 
            if files.ContainsKey(fileName) then
                let (fileWriteTime, fileText) = files.[fileName]
                fileWriteTime
            else 
                dflt.GetLastWriteTimeShim(fileName)

        member __.SafeExists(fileName) = 
            files.ContainsKey(fileName) || dflt.SafeExists(fileName)

        member __.FileStreamCreateShim(fileName) = dflt.FileStreamCreateShim(fileName)
        member __.FileStreamWriteExistingShim(fileName) = dflt.FileStreamWriteExistingShim(fileName)
        member __.GetTempPathShim() = dflt.GetTempPathShim()
        member __.GetFullPathShim(fileName) = dflt.GetFullPathShim(fileName)
        member __.IsInvalidPathShim(fileName) = dflt.IsInvalidPathShim(fileName)
        member __.IsPathRootedShim(fileName) = dflt.IsPathRootedShim(fileName)
        member __.FileDelete(fileName) = dflt.FileDelete(fileName)
        member __.AssemblyLoadFrom(fileName) = dflt.AssemblyLoadFrom fileName
        member __.AssemblyLoad(assemblyName) = dflt.AssemblyLoad assemblyName 

let myFileSystem = MyFileSystem()
Shim.FileSystem <- myFileSystem

let fileName1 = @"c:\mycode\test1.fs" // note, the path doesn't exist

let projectOptions2 = 
    { projectOptions with 
        OtherOptions = [|   yield! projectOptions.OtherOptions |> Array.filter(fun s -> not (s.EndsWith ".fs"))
                            yield fileName1 |] }

let editorWindow = 
    let tb = new TextBox(Text = "")
    tb.AcceptsReturn <- true
    tb.AcceptsTab <- true
    tb

let completionsListBox = 
    let lb = new ListView()
    lb.BorderThickness <- new Thickness(1.)    
    lb

let toolTipWindow = 
    let tb = new TextBlock(Text = "")
    tb


let createEditor() = 
    let sp = new Grid()
    sp.ColumnDefinitions.Add(new ColumnDefinition(Width = new GridLength(4., GridUnitType.Star)))
    sp.ColumnDefinitions.Add(new ColumnDefinition(Width = new GridLength(1., GridUnitType.Star)))
    sp.RowDefinitions.Add(new RowDefinition(Height = new GridLength(4., GridUnitType.Star)))
    sp.RowDefinitions.Add(new RowDefinition(Height = new GridLength(1., GridUnitType.Star)))
    editorWindow.SetValue(Grid.ColumnProperty, 0)
    editorWindow.SetValue(Grid.RowProperty, 0)
    sp.Children.Add(editorWindow) |> ignore
    completionsListBox.SetValue(Grid.ColumnProperty, 1)
    completionsListBox.SetValue(Grid.RowProperty, 0)
    sp.Children.Add(completionsListBox) |> ignore
    toolTipWindow.SetValue(Grid.ColumnProperty, 0)
    toolTipWindow.SetValue(Grid.RowProperty, 1)
    toolTipWindow.SetValue(Grid.ColumnSpanProperty, 2)
    sp.Children.Add(toolTipWindow) |> ignore
    WPF.show(sp)

let getText() =
    let text = editorWindow.Text
    text.Replace("\t", "    ")

let showCompletions completions =
    Application.Current.Dispatcher.Invoke(fun () ->
      completionsListBox.Items.Clear()
      for completion in completions do
         completionsListBox.Items.Add(new ListBoxItem(Content = completion)) |> ignore
    )

let showTooltip text =
    Application.Current.Dispatcher.Invoke(fun () ->
      toolTipWindow.Text <- text
    )

createEditor()

async { while true do 
          try 
            do! Async.Sleep 1000

            let text = getText()
            do myFileSystem.SetFile(fileName1, text) 

            printfn "checking..."

            let! (parseResults, checkResults) =  
                checker.ParseAndCheckFileInProject(fileName1, 0, text, projectOptions2) 
            
            let checkResult = 
                match checkResults with
                | FSharpCheckFileAnswer.Succeeded(res) -> res
                | res -> failwithf "Parsing did not finish... (%A)" res

            let lineIndex = editorWindow.GetLineIndexFromCharacterIndex(editorWindow.CaretIndex)
            let lineStartIndex = editorWindow.GetCharacterIndexFromLineIndex(lineIndex)
            let lineOffset = editorWindow.CaretIndex - lineStartIndex
            let line = editorWindow.GetLineText(lineIndex)
            let scope = line.Substring(line.LastIndexOf(" "))

            let (scope, components) = 
                if not(String.IsNullOrWhiteSpace(scope))
                then
                    match scope.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries) with
                    | [||] -> scope, []
                    | scopes -> scopes.[scopes.Length - 1], scopes.[0..(scopes.Length - 2)] |> Array.toList
                else scope,[]

            printfn "Index: %d, Start: %d, Offset: %d, Scope: %s, Comps %A" lineIndex lineStartIndex lineOffset scope components
            let identToken = Parser.tagOfToken(Parser.token.IDENT("")) 

            // Get tool tip at the specified location
            let! tip = checkResult.GetToolTipTextAlternate(lineIndex,  lineOffset, line, [scope], identToken)

            let! methodGroup = 
                checkResult.GetDeclarationListInfo(Some parseResults, lineIndex,  lineOffset, line, components, scope, fun _ -> false)

            showCompletions (methodGroup.Items |> Array.map (fun decl -> decl.Name))
            showTooltip (sprintf "%A" tip)

            if checkResult.Errors.Length = 0 then 
               printfn "all ok!" 

            for e in checkResult.Errors do 
               printfn "error/warning: %s(%d%d): %s" e.FileName e.StartLineAlternate e.StartColumn e.Message 

          with e -> 
              printfn "whoiops...: %A" e.Message }
   |> Async.StartImmediate