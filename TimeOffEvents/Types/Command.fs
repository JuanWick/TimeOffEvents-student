namespace TimeOff

open TimeOff.User
open TimeOff.TimeOffRequest
open System

module Command = 
    type Command =
        | RequestTimeOff of TimeOffRequest
        | ValidateRequest of UserId * Guid with
        member this.UserId =
            match this with
            | RequestTimeOff request -> request.UserId
            | ValidateRequest (userId, _) -> userId