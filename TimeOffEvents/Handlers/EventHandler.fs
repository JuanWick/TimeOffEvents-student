namespace TimeOff

open TimeOff.RequestState
open TimeOff.RequestEvent
open TimeOff.UserRequestsState
open TimeOff.TimeOffRequest
open TimeOff.IDateProvider

module EventHandler =
    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)