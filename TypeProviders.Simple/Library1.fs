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

    let createErasedType (t : Type) =   
           let ty = ProvidedTypeDefinition(t.Name, Some(t))
           
           let getParameters (m : MethodBase) =
               m.GetParameters() 
               |> Seq.map (fun pi -> ProvidedParameter(pi.Name, pi.ParameterType, pi.IsOut, pi.IsOptional))
               |> Seq.toList
       
           let getMembers (mi : MemberInfo) =
               match mi with
               | :? MethodInfo as m ->  
                   ProvidedMethod(methodName = m.Name,
                                  parameters = getParameters(m), 
                                  returnType = m.ReturnType,
                                  InvokeCode = (fun args -> Expr.Call(Expr.Coerce(args.Head, mi.DeclaringType), m, args.Tail))
                                  ) :> MemberInfo
               | :? ConstructorInfo as c ->
                   ProvidedConstructor(getParameters c, 
                                       InvokeCode = (fun args -> Expr.NewObject(c, args))
                                       ) :> MemberInfo
               | :? PropertyInfo as p ->
                   ProvidedProperty(propertyName = p.Name,
                                    propertyType = p.PropertyType,
                                    GetterCode = (fun args -> Expr.PropertyGet(Expr.Coerce(args.Head, mi.DeclaringType), p, args.Tail)),
                                    SetterCode = (fun args -> Expr.PropertySet(p, args.Head, args.Tail))) :> MemberInfo
    
           let getMembers (t : Type) = 
               t.GetMembers() |> Seq.map getMembers |> Seq.toList
           
           ty.AddMembers(getMembers t)
           ty 

    let resolveAssembly path (rargs : ResolveEventArgs) =
        let path = Path.GetDirectoryName(path)
        let name = Path.Combine(path, rargs.Name + ".dll")
        if File.Exists(name) 
        then Assembly.LoadFrom(name)
        else null

    let populateAssembly (reqType:ProvidedTypeDefinition) assemblyPath =
        let name = Path.GetFileNameWithoutExtension(assemblyPath)
        System.AppDomain.CurrentDomain.add_AssemblyResolve(new ResolveEventHandler(fun _ a -> resolveAssembly assemblyPath a))
        let assembly = ProvidedTypeDefinition.RegisterGenerated(assemblyPath)
        
        for t in assembly.GetExportedTypes() do
            reqType.AddMember(createErasedType t)
       
        reqType

    let buildAssembly (typeName:string) (args:obj[]) = 
        let reqType = ProvidedTypeDefinition(ass, ns, typeName, Some typeof<obj>)
        let assemblyPath = args.[0] :?> string
        Helpers.memoize (populateAssembly reqType) assemblyPath

    do hostType.DefineStaticParameters(parameters, buildAssembly)
    
    do 
        this.RegisterRuntimeAssemblyLocationAsProbingFolder(cfg)
        this.AddNamespace(ns, [hostType])

[<TypeProviderAssembly>]
do()
       

