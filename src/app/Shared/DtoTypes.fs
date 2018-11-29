namespace TimeOff

open TimeOff.DomainTypes
open System

module DtoTypes =

    [<CLIMutable>]
    type TimeOffRequestAskDto = {
        UserId: UserId
        Start: Boundary
        End: Boundary
    }

    [<CLIMutable>]
    type UserAndRequestId = {
        UserId: UserId
        RequestId: Guid
    }