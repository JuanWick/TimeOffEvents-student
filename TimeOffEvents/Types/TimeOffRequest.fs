namespace TimeOff

open System
open TimeOff.User
open TimeOff.Boundary

module TimeOffRequest =

    type TimeOffRequest = {
        UserId: UserId
        RequestId: Guid
        Start: Boundary
        End: Boundary
    }