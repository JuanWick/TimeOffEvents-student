namespace TimeOff

open Expecto

module TestsRunner =

    [<EntryPoint>]
    let main args =
        runTestsInAssembly { defaultConfig with ``parallel`` = false } args