namespace TimeOff

open TimeOff.DomainTypes

module DtoTypes =

    [<CLIMutable>]
    type TimeOffRequestAskDto = {
        UserId: UserId
        Start: Boundary
        End: Boundary
    }