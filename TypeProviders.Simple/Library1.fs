namespace TypeProviders.Simple

open Microsoft.FSharp.Core.CompilerServices
open Samples.FSharp.ProvidedTypes
open System.Reflection
open System
open System.IO
open Microsoft.FSharp.Quotations
open System.Collections.Generic

module Helpers = 

    type private Cache private() =
       static let mutable instance = Dictionary<_, _>()
       static member Instance = instance

    let internal memoize f =
        fun n ->
            match Cache.Instance.TryGetValue(n) with
            | (true, v) -> v
            | _ ->
                let temp = f(n)
                Cache.Instance.Add(n, temp)
                temp

[<TypeProvider>]
type public SimpleTypeProvider(cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces() 
    
    let ns = "Inno"
    let ass = Assembly.GetExecutingAssembly()
    
    let hostType = ProvidedTypeDefinition(ass, ns, "InternalisingProvider", Some(typeof<obj>), HideObjectMethods = true)

    let parameters = 
        [
            ProvidedStaticParameter("AssemblyPath", typeof<string>)
        ]

    let provideAssembly (reqType:ProvidedTypeDefinition) assemblyPath =
        reqType.AddAssemblyTypesAsNestedTypesDelayed(fun() -> Assembly.LoadFrom assemblyPath)
        reqType

    let buildAssembly (typeName:string) (args:obj[]) = 
        let reqType = ProvidedTypeDefinition(ass, ns, typeName, Some typeof<obj>)
        let assemblyPath = args.[0] :?> string
        Helpers.memoize (provideAssembly reqType) assemblyPath

    do hostType.DefineStaticParameters(parameters, buildAssembly)
    
    do 
        this.RegisterRuntimeAssemblyLocationAsProbingFolder(cfg)
        this.AddNamespace(ns, [hostType])

[<TypeProviderAssembly>]
do()
       

