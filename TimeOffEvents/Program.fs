namespace TimeOff

open Expecto
open TimeOff.Tests

module TestsRunner =

    [<EntryPoint>]
    let main args =
        runTestsInAssembly { defaultConfig with ``parallel`` = false } args