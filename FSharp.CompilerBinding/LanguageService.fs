﻿namespace FSharp.CompilerBinding
open System
open System.IO
open System.Diagnostics
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

module Symbols =
  /// We always know the text of the identifier that resolved to symbol.
  /// Trim the range of the referring text to only include this identifier.
  /// This means references like A.B.C are trimmed to "C".  This allows renaming to just rename "C". 
  let trimSymbolRegion(symbolUse:FSharpSymbolUse) (lastIdentAtLoc:string) =
    let m = symbolUse.RangeAlternate 
    let ((beginLine, beginCol), (endLine, endCol)) = ((m.StartLine, m.StartColumn), (m.EndLine, m.EndColumn))
             
    let (beginLine, beginCol) =
        if endCol >=lastIdentAtLoc.Length && (beginLine <> endLine || (endCol-beginCol) >= lastIdentAtLoc.Length) then 
            (endLine,endCol-lastIdentAtLoc.Length)
        else
            (beginLine, beginCol)
    (beginLine, beginCol), (endLine, endCol)

/// Contains settings of the F# language service
module ServiceSettings =
  let internal getEnvInteger e dflt = match System.Environment.GetEnvironmentVariable(e) with null -> dflt | t -> try int t with _ -> dflt
  /// When making blocking calls from the GUI, we specify this value as the timeout, so that the GUI is not blocked forever
  let blockingTimeout = getEnvInteger "FSharpBinding_BlockingTimeout" 250
  let maximumTimeout = getEnvInteger "FSharpBinding_MaxTimeout" 5000

// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips). Provides default
/// empty/negative results if information is missing.
type ParseAndCheckResults private (infoOpt: (FSharpCheckFileResults * FSharpParseFileResults) option) =
    let token = Parser.tagOfToken(Parser.token.IDENT("")) 

    new (checkResults, parseResults) = ParseAndCheckResults(Some (checkResults, parseResults))

    static member Empty = ParseAndCheckResults(None)

    /// Get declarations at the current location in the specified document and the long ident residue
    /// e.g. The incomplete ident One.Two.Th will return Th
    member x.GetDeclarations(line, col, lineStr) = 
        match infoOpt with 
        | None -> None
        | Some (checkResults, parseResults) -> 
            let longName,residue = Parsing.findLongIdentsAndResidue(col, lineStr)
            Debug.WriteLine (sprintf "GetDeclarations: '%A', '%s'" longName residue)
            // Get items & generate output
            try
             let results =
                 Async.RunSynchronously (checkResults.GetDeclarationListInfo(Some parseResults, line, col, lineStr, longName, residue, fun (_,_) -> false),
                                         timeout = ServiceSettings.blockingTimeout )
             Some (results, residue)
            with :? TimeoutException -> None

    /// Get the symbols for declarations at the current location in the specified document and the long ident residue
    /// e.g. The incomplete ident One.Two.Th will return Th
    member x.GetDeclarationSymbols(line, col, lineStr) = 
        match infoOpt with 
        | None -> None
        | Some (checkResults, parseResults) -> 
            let longName,residue = Parsing.findLongIdentsAndResidue(col, lineStr)
            Debug.WriteLine (sprintf "GetDeclarationSymbols: '%A', '%s'" longName residue)
            // Get items & generate output
            try
             let results = 
                 Async.RunSynchronously (checkResults.GetDeclarationListSymbols(Some parseResults, line, col, lineStr, longName, residue, fun (_,_) -> false),
                                         timeout = ServiceSettings.blockingTimeout )
             Some (results, residue)
            with :? TimeoutException -> None

    /// Get the tool-tip to be displayed at the specified offset (relatively
    /// from the beginning of the current document)
    member x.GetToolTip(line, col, lineStr) =
      async {
        match infoOpt with 
        | None -> return None
        | Some (checkResults, _parseResults) -> 
        match Parsing.findLongIdents(col, lineStr) with 
        | None -> return None
        | Some(col,identIsland) ->
          let! res = checkResults.GetToolTipTextAlternate(line, col, lineStr, identIsland, token)
          let! sym = checkResults.GetSymbolUseAtLocation(line, col, lineStr, identIsland)
          Debug.WriteLine("Result: Got something, returning")
          return sym |> Option.bind (fun sym -> let (_, startCol), (_, endCol) = Symbols.trimSymbolRegion sym (Seq.last identIsland)
                                                Some (res, (startCol, endCol)))
      }
    member x.GetDeclarationLocation(line, col, lineStr) =
      async {
        match infoOpt with 
        | None -> return FSharpFindDeclResult.DeclNotFound FSharpFindDeclFailureReason.Unknown
        | Some (checkResults, _parseResults) -> 
        match Parsing.findLongIdents(col, lineStr) with 
        | None -> return FSharpFindDeclResult.DeclNotFound FSharpFindDeclFailureReason.Unknown
        | Some(col,identIsland) -> return! checkResults.GetDeclarationLocationAlternate(line, col, lineStr, identIsland, false)
      }
    member x.GetMethods(line, col, lineStr) =
      async { 
        match infoOpt with 
        | None -> return None
        | Some (checkResults, _parseResults) -> 
        match Parsing.findLongIdentsAtGetMethodsTrigger(col, lineStr) with 
        | None -> return None
        | Some(col,identIsland) ->
            let! res = checkResults.GetMethodsAlternate(line, col, lineStr, Some identIsland)
            Debug.WriteLine("Result: Got something, returning")
            return Some (res.MethodName, res.Methods) 
      }

    member x.GetSymbol(line, col, lineStr) =
      async {
        match infoOpt with 
        | None -> return None
        | Some (checkResults, _parseResults) -> 
        match Parsing.findLongIdents(col, lineStr) with 
        | None -> return None
        | Some(colu, identIsland) ->
            return! checkResults.GetSymbolUseAtLocation(line, colu, lineStr, identIsland)
      }

    member x.GetSymbolAtLocation(line, col, lineStr, identIsland) =
      async {
        match infoOpt with 
        | None -> return None
        | Some (checkResults, _parseResults) -> 
            return! checkResults.GetSymbolUseAtLocation (line, col, lineStr, identIsland)
      }

    member x.GetUsesOfSymbolInFile(symbol) =
      async {
        match infoOpt with 
        | None -> return [| |]
        | Some (checkResults, _parseResults) -> return! checkResults.GetUsesOfSymbolInFile(symbol)
      }

    member x.GetAllUsesOfAllSymbolsInFile() =
      async {
          match infoOpt with
          | None -> return None
          | Some (checkResults, _parseResults) ->
              let! allSymbols = checkResults.GetAllUsesOfAllSymbolsInFile()
              return Some allSymbols
      }

    member x.PartialAssemblySignature =
      async {
          match infoOpt with
          | None -> return None
          | Some (checkResults, _parseResults) ->
              return Some checkResults.PartialAssemblySignature
      }

    member x.GetErrors() =
        match infoOpt with 
        | None -> None
        | Some (checkResults, _parseResults) -> Some checkResults.Errors

    member x.GetNavigationItems() =
        match infoOpt with 
        | None -> [| |]
        | Some (_checkResults, parseResults) -> 
           // GetNavigationItems is not 100% solid and throws occasional exceptions
            try parseResults.GetNavigationItems().Declarations
            with _ -> 
                Debug.Assert(false, "couldn't update navigation items, ignoring")  
                [| |]

    member x.ParseTree = 
        match infoOpt with
        | Some (_checkResults,parseResults) -> parseResults.ParseTree
        | None -> None

    member x.CheckResults = 
        match infoOpt with
        | Some (checkResults,_parseResults) -> checkResults |> Some
        | None -> None

    member x.GetExtraColorizations() =
        match infoOpt with
        | Some(parseResults,_checkResults) -> parseResults.GetExtraColorizationsAlternate() |> Some
        | None -> None

[<RequireQualifiedAccess>]
type AllowStaleResults = 
    // Allow checker results where the source doesn't even match
    | MatchingFileName
    // Allow checker results where the source matches but where the background builder may not have caught up yet after some other change
    | MatchingSource
    // Don't allow stale results
    | No

  
// --------------------------------------------------------------------------------------
// Language service 

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService(dirtyNotify) =

  /// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
  /// Not yet sure if this works for scripts.
  let fakeDateTimeRepresentingTimeLoaded proj = DateTime(abs (int64 (match proj with null -> 0 | _ -> proj.GetHashCode())) % 103231L)

  // Create an instance of interactive checker. The callback is called by the F# compiler service
  // when its view of the prior-typechecking-state of the start of a file has changed, for example
  // when the background typechecker has "caught up" after some other file has been changed, 
  // and its time to re-typecheck the current file.
  let checker = 
    let checker = FSharpChecker.Create()
    checker.BeforeBackgroundFileCheck.Add dirtyNotify
    checker

  /// When creating new script file on Mac, the filename we get sometimes 
  /// has a name //foo.fsx, and as a result 'Path.GetFullPath' throws in the F#
  /// language service - this fixes the issue by inventing nicer file name.
  let fixFileName path = 
    if (try Path.GetFullPath(path) |> ignore; true
        with _ -> false) then path
    else 
      let dir = 
        if Environment.OSVersion.Platform = PlatformID.Unix ||  
           Environment.OSVersion.Platform = PlatformID.MacOSX then
          Environment.GetEnvironmentVariable("HOME") 
        else
          Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%")
      Path.Combine(dir, Path.GetFileName(path))
   
  // We use a mailbox processor to wrap requests to F.C.S. here so 
  //   (a) we can get work off the GUI thread and 
  //   (b) timeout on synchronous requests. 
  // 
  // There is already a background compilation queue+thread in F.C.S. abd many of the F.C.S. operations
  // are already asynchronous. However using direct calls to the F.C.S. API isn't quite sufficient because
  // we can't timeout, and not all F.C.S. operations are asynchronous (e.g. parsing). If F.C.S. is extended
  // so all operations are asynchronous then I believe we won't need a wrapper agent at all.
  //
  // Every request to this agent is 'PostAndReply' or 'PostAndAsyncReply'.  This means the requests are
  // a lot like a function call, except 
  //   (a) they may be asynchronous (reply is interleaved on the UI thread)
  //   (b) they may be on on a timeout (to prevent blocking the UI thread)
  //   (c) only one request is active at a time, the rest are in the queue

  let mbox = MailboxProcessor.Start(fun mbox ->
    
    async { 
       while true do
            try
              Debug.WriteLine("LanguageService agent: Awaiting request") 
              let! (fileName, source, options, reply: AsyncReplyChannel<_> ) = mbox.Receive()

              Debug.WriteLine("LanguageService agent: Dequeued request {0} remaining", mbox.CurrentQueueLength) 
              let fileName = fixFileName(fileName)

              Debug.WriteLine("LanguageService agent: start parsing - {0}", box fileName)
              let! parseResults = checker.ParseFileInProject(fileName, source, options)
              Debug.WriteLine("LanguageService agent: parse completed")

              Debug.WriteLine("LanguageService agent: Typecheck source...")
              let! checkAnswer = checker.CheckFileInProject(parseResults, fileName, 0, source,options, IsResultObsolete(fun () -> false), null )
              Debug.WriteLine(sprintf "LanguageService agent: Typecheck completed")
              
              // Construct new typed parse result if the task succeeded
              let results =
                match checkAnswer with
                | FSharpCheckFileAnswer.Succeeded(checkResults) ->
                    Debug.WriteLine(sprintf "LanguageService agent: Update typed info - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo)
                    ParseAndCheckResults(checkResults, parseResults)
                | _ -> 
                    Debug.WriteLine("LanguageService agent: Update typed info - failed")
                    ParseAndCheckResults.Empty
                    
              reply.Reply results
            with exn -> Debug.WriteLine( sprintf "LanguageService agent: Exception: %s" (exn.ToString()) )
        })

  static member IsAScript fileName =
      let ext = Path.GetExtension fileName
      [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  member x.GetCheckerOptions(fileName, projFilename, source, files, args) =
    let opts =
      if LanguageService.IsAScript fileName then
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        x.GetScriptCheckerOptions(fileName, projFilename, source)
          
      // We are in a project - construct options using current properties
      else
        x.GetProjectCheckerOptions(projFilename, files, args)
    opts
   
  /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
  member x.GetScriptCheckerOptions(fileName, projFilename, source) =
    let opts = 
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
          let fileName = fixFileName(fileName)
          Debug.WriteLine (sprintf "LanguageService: GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName )
          let opts =
              Async.RunSynchronously (checker.GetProjectOptionsFromScript(fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename),
                                      timeout = ServiceSettings.maximumTimeout)

          // The InteractiveChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
          if opts.OtherOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then opts
          else 
            // Add assemblies that may be missing in the standard assembly resolution
            Debug.WriteLine("LanguageService: GetScriptCheckerOptions: Adding missing core assemblies.")
            let dirs = FSharpEnvironment.getDefaultDirectories (None, FSharpTargetFramework.NET_4_5 )
            {opts with OtherOptions = [| yield! opts.OtherOptions
                                         match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                         | Some fn -> yield sprintf "-r:%s" fn
                                         | None -> Debug.WriteLine("LanguageService: Resolution: FSharp.Core assembly resolution failed!")
                                         match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                                         | Some fn -> yield sprintf "-r:%s" fn
                                         | None -> Debug.WriteLine("LanguageService: Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!") |]}
        with e -> failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e

    // Print contents of check option for debugging purposes
    // Debug.WriteLine(sprintf "GetScriptCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
    //                      opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules)
    opts
   
  /// Constructs options for the interactive checker for a project under the given configuration. 
  member x.GetProjectCheckerOptions(projFilename, files, args) =
    let opts = 
      
      // We are in a project - construct options using current properties
        Debug.WriteLine (sprintf "LanguageService: GetProjectCheckerOptions: Creating for project '%s'" projFilename )

        {ProjectFileName = projFilename
         ProjectFileNames = files
         OtherOptions = args
         ReferencedProjects = [| |]
         IsIncompleteTypeCheckEnvironment = false
         UseScriptResolutionRules = false   
         LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
         UnresolvedReferences = None } 

    // Print contents of check option for debugging purposes
    // Debug.WriteLine(sprintf "GetProjectCheckerOptions: ProjectFileName: %s, ProjectFileNames: %A, ProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
    //                      opts.ProjectFileName opts.ProjectFileNames opts.ProjectOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules)
    opts
    
  
  /// Parses and checks the given file in the given project under the given configuration. Asynchronously
  /// returns the results of checking the file.
  member x.ParseAndCheckFileInProject(projectFilename, fileName:string, src, files, args, storeAst, ?startBgCompile) =
   let startBgCompile = defaultArg startBgCompile true

   async {
    let opts = x.GetCheckerOptions(fileName, projectFilename,  src, files , args)
    Debug.WriteLine(sprintf "LanguageService: ParseAndCheckFileInProject: Trigger parse (fileName=%s)" fileName)

    // storeAst is passed from monodevelop when it finds files with the same name in other projects. 
    // It will then ask for a reparse of all files in the second project. If storeAst = false, do nothing. 
    if not storeAst then return ParseAndCheckResults.Empty else
    let! results = mbox.PostAndAsyncReply(fun r -> fileName, src, opts, r)
    if startBgCompile then
        Debug.WriteLine(sprintf "LanguageService: Starting background compilations")
        checker.StartBackgroundCompile(opts)
    return results
   }

  member x.ParseFileInProject(projectFilename, fileName:string, src, args) = 
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, [| |], args)
    Debug.WriteLine(sprintf "LanguageService: ParseFileInProject: Get untyped parse result (fileName=%s)" fileName)
    checker.ParseFileInProject(fixFileName fileName, src, opts)

  member internal x.TryGetStaleTypedParseResult(fileName:string, options, src, stale)  = 
    // Try to get recent results from the F# service
    let res = 
        match stale with 
        | AllowStaleResults.MatchingFileName -> checker.TryGetRecentTypeCheckResultsForFile(fixFileName fileName, options) 
        | AllowStaleResults.MatchingSource -> checker.TryGetRecentTypeCheckResultsForFile(fixFileName fileName, options, source=src) 
        | AllowStaleResults.No -> None
    match res with 
    | Some (untyped,typed,_) when typed.HasFullTypeCheckInfo  -> Some (ParseAndCheckResults(typed, untyped))
    | _ -> None

  member x.GetTypedParseResultWithTimeout(projectFilename, fileName:string, src, files, args, stale, ?timeout) = 
   async {
    let fileName = if Path.GetExtension fileName = ".sketchfs" then Path.ChangeExtension (fileName, ".fsx") else fileName
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, files, args)
    Debug.WriteLine("LanguageService: Parsing: Get typed parse result, fileName={0}", box fileName)
    // Try to get recent results from the F# service
    match x.TryGetStaleTypedParseResult(fileName, opts, src, stale) with
    | Some _ as results ->
        Debug.WriteLine(sprintf "LanguageService: Parsing: using stale results")
        return results
    | None -> 
        Debug.WriteLine(sprintf "LanguageService: Not using stale results - trying typecheck with timeout")
        // If we didn't get a recent set of type checking results, we put in a request and wait for at most 'timeout' for a response
        match timeout with
        | Some timeout -> return mbox.TryPostAndReply((fun reply -> (fileName, src, opts, reply)), timeout = timeout)
        | None -> return mbox.TryPostAndReply((fun reply -> (fileName, src, opts, reply)))
   }

  member x.GetTypedParseResultIfAvailable(projectFilename, fileName:string, src, files, args, stale) = 
    let opts = x.GetCheckerOptions(fileName, projectFilename, src, files, args)
    match x.TryGetStaleTypedParseResult(fileName, opts, src, stale)  with
    | Some results -> results
    | None -> ParseAndCheckResults.Empty


  /// Get all the uses of a symbol in the given file (using 'source' as the source for the file)
  member x.GetUsesOfSymbolAtLocationInFile(projectFilename, fileName, source, files, line:int, col, lineStr, args) =
   async { 
    match FSharp.CompilerBinding.Parsing.findLongIdents(col, lineStr) with 
    | Some(colu, identIsland) ->
        let! checkResults = x.GetTypedParseResultWithTimeout(projectFilename, fileName, source, files, args, stale= AllowStaleResults.MatchingSource)
        match checkResults with
        | Some results ->
            let! symbolResults = results.GetSymbolAtLocation(line, colu, lineStr, identIsland)
            match symbolResults with
            | Some symbolUse -> 
                let lastIdent = Seq.last identIsland
                let! refs = results.GetUsesOfSymbolInFile(symbolUse.Symbol)
                return Some(lastIdent, refs)
            | None -> return None
        | None -> return None
    | None -> return None 
   }

  member x.GetUsesOfSymbolInProject(projectFilename, file, source, files, args, symbol:FSharpSymbol) =
   async { 
    let projectOptions = x.GetCheckerOptions(file, projectFilename, source, files, args)

    //parse and retrieve Checked Project results, this has the entity graph and errors etc
    let! projectResults = checker.ParseAndCheckProject(projectOptions) 
  
    let! refs = projectResults.GetUsesOfSymbol(symbol)
    return refs }

  member x.InvalidateConfiguration(options) = checker.InvalidateConfiguration(options)

  member x.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients() =
      checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
