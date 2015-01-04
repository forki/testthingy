module testthingy  

open System

type IReporter =
   abstract member testStart : string -> unit
   abstract member pass : unit -> unit
   abstract member fail : Exception -> string -> unit
   abstract member todo : unit -> unit
   abstract member skip : unit -> unit
   abstract member testEnd : string -> unit
   abstract member describe : string -> unit
   abstract member contextStart : string -> unit
   abstract member contextEnd : string -> unit
   abstract member summary : int -> int -> int -> int -> unit
   abstract member write : string -> unit
   abstract member suggestSelectors : string -> string list -> unit
   abstract member quit : unit -> unit
   abstract member suiteBegin : unit -> unit
   abstract member suiteEnd : unit -> unit
   abstract member coverage : string -> byte [] -> unit

type ConsoleReporter() =   
    interface IReporter with               
        member this.pass () = 
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine("Passed");
            Console.ResetColor()

        member this.fail ex id = 
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("Error: ");
            Console.ResetColor()
            Console.WriteLine(ex.Message);
            Console.WriteLine("Stack: ");
            ex.StackTrace.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
            |> Array.iter (fun trace -> 
                Console.ResetColor()
                if trace.Contains(".FSharp.") then
                    Console.WriteLine(trace)
                else
                    if trace.Contains(":line") then
                        let beginning = trace.Split([| ":line" |], StringSplitOptions.None).[0]
                        let line = trace.Split([| ":line" |], StringSplitOptions.None).[1]
                        Console.Write(beginning)
                        Console.ForegroundColor <- ConsoleColor.DarkGreen
                        Console.WriteLine(":line" + line)
                    else
                        Console.ForegroundColor <- ConsoleColor.DarkGreen
                        Console.WriteLine(trace)
                Console.ResetColor())

        member this.describe d = Console.WriteLine d          
        
        member this.contextStart c = Console.WriteLine (String.Format("context: {0}", c))        
        
        member this.contextEnd c = ()
        
        member this.summary minutes seconds passed failed =
            Console.WriteLine()
            Console.WriteLine("{0} minutes {1} seconds to execute", minutes, seconds)
            if failed = 0 then
                Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine("{0} passed", passed)
            Console.ResetColor()
            if failed > 0 then
                Console.ForegroundColor <- ConsoleColor.Red        
            Console.WriteLine("{0} failed", failed)    
            Console.ResetColor()        
        
        member this.write w = Console.WriteLine w        
        
        member this.suggestSelectors selector suggestions = 
            Console.ForegroundColor <- ConsoleColor.DarkYellow                    
            Console.WriteLine("Couldnt find any elements with selector '{0}', did you mean:", selector)
            suggestions |> List.iter (fun suggestion -> Console.WriteLine("\t{0}", suggestion))
            Console.ResetColor()
        member this.testStart id =
            Console.ForegroundColor <- ConsoleColor.DarkCyan
            Console.WriteLine("Test: {0}", id)
            Console.ResetColor()

        member this.testEnd id = ()
        
        member this.quit () = ()
        
        member this.suiteBegin () = ()

        member this.suiteEnd () = ()

        member this.coverage url ss = ()
        
        member this.todo () = ()

        member this.skip () = () 

let rec private last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

type Test (description: string, func : (unit -> unit), number : int) =
    member x.Description = description
    member x.Func = func
    member x.Number = number

type suite () = class
    let mutable context : string = null
    let mutable once = fun () -> ()
    let mutable before = fun () -> ()
    let mutable after = fun () -> ()
    let mutable lastly = fun () -> () 
    let mutable tests : Test list = []

    member x.Context
        with get() = context
        and set(value) = context <- value
    member x.Once
        with get() = once
        and set(value) = once <- value
    member x.Before
        with get() = before
        and set(value) = before <- value
    member x.After
        with get() = after
        and set(value) = after <- value
    member x.Lastly
        with get() = lastly
        and set(value) = lastly <- value
    member x.Tests
        with get() = tests
        and set(value) = tests <- value
end

//test runner
let failFast = ref false
let mutable reporter : IReporter = new ConsoleReporter() :> IReporter
let mutable suites = [new suite()]
let mutable todo = fun () -> ()
let mutable skipped = fun () -> ()

let once f = (last suites).Once <- f
let before f = (last suites).Before <- f
let after f = (last suites).After <- f
let lastly f = (last suites).Lastly <- f
let context c = 
    if (last suites).Context = null then 
        (last suites).Context <- c
    else 
        let s = new suite()
        s.Context <- c
        suites <- suites @ [s]

let ( &&& ) description f = (last suites).Tests <- (last suites).Tests @ [Test(description, f, (last suites).Tests.Length + 1)]
let ( &&! ) description f = (last suites).Tests <- (last suites).Tests @ [Test(description, skipped, (last suites).Tests.Length + 1)]

let mutable passedCount = 0
let mutable failedCount = 0
let mutable contextFailed = false
let mutable failedContexts : string list = []
let mutable failed = false
let mutable (failureMessage : string) = null

let pass () =    
    passedCount <- passedCount + 1
    reporter.pass ()

let fail (ex : Exception) id =
    if failFast = ref true then failed <- true        
    failedCount <- failedCount + 1
    contextFailed <- true
    let p = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + @"\canopy\"
    let f = DateTime.Now.ToString("MMM-d_HH-mm-ss-fff")
    reporter.fail ex id

let run () =
    reporter.suiteBegin()
    let stopWatch = new Diagnostics.Stopwatch()
    stopWatch.Start()      
    
    let runtest (suite : suite) (test : Test) =
        if failed = false then
            let desc = if test.Description = null then (String.Format("Test #{0}", test.Number)) else test.Description
            try 
                reporter.testStart desc  
                if System.Object.ReferenceEquals(test.Func, todo) then 
                    reporter.todo ()
                else if System.Object.ReferenceEquals(test.Func, skipped) then 
                    reporter.skip ()
                else
                    suite.Before ()
                    test.Func ()
                    suite.After ()
                    pass()
            with
                | ex when failureMessage <> null && failureMessage = ex.Message -> pass()
                | ex -> fail ex desc
            reporter.testEnd desc
        
        failureMessage <- null            
            
    suites
    |> List.iter (fun s ->
        if failed = false then
            contextFailed <- false
            if s.Context <> null then reporter.contextStart s.Context
            s.Once ()            
            s.Tests |> List.iter (fun t -> runtest s t)
            s.Lastly ()        
            if contextFailed = true then failedContexts <- failedContexts @ [s.Context]
            if s.Context <> null then reporter.contextEnd s.Context
    )
    
    stopWatch.Stop()    
    reporter.summary stopWatch.Elapsed.Minutes stopWatch.Elapsed.Seconds passedCount failedCount 
    reporter.suiteEnd()
    
//assertions
//add your own or open an issue and I will add more
let (==) value1 value2 =  if value1 <> value2 then raise <| Exception(sprintf "expected: %A\r\ngot: %A" value2 value1)
let (!=) value1 value2 =  if value1 = value2 then raise <| Exception(sprintf "got: %A\r\nwhen expecting not to" value1)