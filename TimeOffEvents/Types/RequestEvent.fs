namespace TimeOff

open TimeOff.TimeOffRequest

module RequestEvent =
    type RequestEvent =
        | RequestCreated of TimeOffRequest
        | RequestValidated of TimeOffRequest with
        member this.Request =
            match this with
            | RequestCreated request -> request
            | RequestValidated request -> request