// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let[<Literal>]assemblyPath = @"D:\Appdev\Playground\SimpleLib\bin\Debug\SimpleLib.dll"
type T = Inno.InternalisingProvider<assemblyPath>

[<EntryPoint>]
let main argv = 
    let c = new T.Class1()
    printfn "Result: %A" c.X
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
