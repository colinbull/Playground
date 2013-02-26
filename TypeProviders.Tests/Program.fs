// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

let[<Literal>]assemblyPath = @"D:\Appdev\Playground\SimpleLib\bin\Debug\SimpleLib.dll"
type T = Inno.InternalisingProvider<assemblyPath>
type Greeter = T.SimpleLib.Greeter

[<EntryPoint>]
let main argv = 
    let c = new Greeter("Colin")
    printfn "%s" (c.Say("Hello"))
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
