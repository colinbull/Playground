// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let[<Literal>]assemblyPath = @"D:\Appdev\Playground\SimpleLib\bin\Debug\SimpleLib.dll"
type T = Inno.InternalisingProvider<assemblyPath>
type C = T.Class1

[<EntryPoint>]
let main argv = 
    let c = new C()
    printfn "Result: %A" c.X
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
