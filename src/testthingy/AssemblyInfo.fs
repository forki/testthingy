namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("testthingy")>]
[<assembly: AssemblyProductAttribute("testthingy")>]
[<assembly: AssemblyDescriptionAttribute("a really simple f# unit test library")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
