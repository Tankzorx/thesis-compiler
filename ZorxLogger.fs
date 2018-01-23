namespace Zorx




module Logging =

    // Enable the creation of multiple loggers
    // that are independent on each others
    // enabled/disabled state.
    let logger enabled msg =
        if enabled then
            printfn "%A" msg
        else
            ()