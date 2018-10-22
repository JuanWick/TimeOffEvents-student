namespace TimeOff

open System
open TimeOff.User

module TimeOffRequest =
    type HalfDay = | AM | PM

    type Boundary = {
        Date: DateTime
        HalfDay: HalfDay
    }


    type TimeOffRequest = {
        UserId: UserId
        RequestId: Guid
        Start: Boundary
        End: Boundary
    }