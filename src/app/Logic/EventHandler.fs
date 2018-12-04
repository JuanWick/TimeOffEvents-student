namespace TimeOff

open TimeOff.DomainTypes

module EventHandler =
    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestRefused request -> Refused request
        | RequestCanceledByEmployee request -> CanceledByEmployee request
        | RequestAskedCancel request -> AskCanceled request
        | RequestCancelRefused request -> CancelRefused request
        | RequestCanceledByManager request -> CanceledByManager request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)