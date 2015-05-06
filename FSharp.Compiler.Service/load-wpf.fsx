#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "WindowsBase.dll"
#r "System.Xaml.dll"

namespace FsiWpf

open System.Windows
open System.Windows.Threading

module WPF =

    let install() = 
        fsi.EventLoop <-  
            let app  = 
                match Application.Current with 
                | null -> let app = new Application()  in new Window() |> ignore; app
                | app -> app
        
            app.DispatcherUnhandledException.Add(fun args -> 
                eprintfn ""
                eprintfn "Error in WPF Event Handler: %O" args.Exception
                eprintfn ""
                args.Handled <- true )
        
            { new Microsoft.FSharp.Compiler.Interactive.IEventLoop with
                 member x.Run() = app.Run() |> ignore; false
                 member x.Invoke f = 
                     try app.Dispatcher.Invoke(DispatcherPriority.Send,System.Func<obj>(f >> box)) |> unbox
                     with e -> eprintf "\n\n ERROR: %O\n" e; reraise()
                 member x.ScheduleRestart() =   () }
    
    let show content =
        if Application.Current = null then install()
        Application.Current.MainWindow.Content <- content
        Application.Current.MainWindow.Show() 
    