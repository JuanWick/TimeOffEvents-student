namespace TimeOff

open TimeOff.DomainTypes
open TimeOff.ICustomDate

module CommandHandler =
    open System

    let overlapsWith request1 request2 = //AM = matin | PM = après-midi

        if request1.Start.Date >= request2.Start.Date && request1.End.Date <= request2.End.Date then
            true
        elif request1.Start.Date <= request2.Start.Date && request1.End.Date >= request2.Start.Date then
            if request1.End.Date = request2.Start.Date then
                if request1.End.HalfDay = HalfDay.AM && request2.Start.HalfDay = HalfDay.PM then
                    false
                else
                    true
            else
                true
        elif request1.Start.Date <= request2.End.Date then
            if request1.Start.Date = request2.End.Date then
                if request1.Start.HalfDay = HalfDay.AM && request2.End.HalfDay = HalfDay.PM then
                    true
                elif request1.Start.HalfDay = request2.End.HalfDay then
                    true
                else
                    false
            else
                false
        else
            false

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let results: bool list =
            otherRequests
            |> Seq.toList
            |> List.map (fun x -> overlapsWith request x)
            |> List.filter (fun x -> x)

        results.Length > 0

    let getRequestById requestState =
        
        match requestState with
        | PendingValidation request ->
            Ok [RequestCreated request]
        | Validated request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be found"

    ////////////////////////////////////////////////////////////////////////////

    let createRequest activeUserRequests  request dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate

        if overlapsWithAnyRequest activeUserRequests  request then
            Error "Overlapping request"
        elif request.Start.Date <= dateProvider.Now() then
            Error "The request starts in the past"
        else 
            Ok [RequestCreated request]

    let employeeCancelRequest (requestState:RequestState) dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate
        let request = requestState.Request

        if request.Start.Date <= dateProvider.Now() then
            Error "The request starts in the past"
        else
            match requestState with
            | PendingValidation request-> 
                Ok [RequestCanceledByEmployee request]
            | Validated request ->
                Ok [RequestCanceledByEmployee request]
            | _ -> Error "Invalid state for action"

    let refuseRequest (requestState:RequestState) dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate
        match requestState with
        | PendingValidation request-> 
            if request.Start.Date <= dateProvider.Now() then
                Error "The request starts in the past"
            else
                Ok [RequestRefused request]
        | _ -> Error "Invalid state for action"

    let askCancelRequest (requestState:RequestState) dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate
        let request = requestState.Request
        if request.Start.Date <= dateProvider.Now() then
            match requestState with
            | Validated request-> 
                Ok [RequestAskedCancel request]
            | _ -> Error "Invalid state for action"
        else
            Error "The request starts in the futur"

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Invalid state for action"

    let refuseCanceledRequest requestState =
         match requestState with
            | AskCanceled request-> 
                    Ok [RequestCancelRefused request]
            | _ -> Error "Invalid state for action"

    let managerCancelRequest requestState =
         match requestState with
            | PendingValidation request-> 
                    Ok [RequestCanceledByManager request]
            | Validated request-> 
                    Ok [RequestCanceledByManager request]
            | AskCanceled request-> 
                    Ok [RequestCanceledByManager request]
            | CancelRefused request-> 
                    Ok [RequestCanceledByManager request]
            | _ -> Error "Invalid state for action"
         
    ////////////////////////////////////////////////////////////////////////////

    let convertStates(requestState:RequestState)  =
        match requestState with
        | NotCreated -> None
        | PendingValidation request ->
             Some (RequestCreated request)
        | Validated request ->
             Some (RequestValidated request)
        | Refused request -> 
             Some (RequestRefused request)
        | CanceledByEmployee request ->
             Some (RequestCanceledByEmployee request)
        | AskCanceled request ->
             Some(RequestAskedCancel request)
        | CancelRefused request ->
             Some(RequestCancelRefused request)
        | CanceledByManager request ->
             Some(RequestCanceledByManager request)

    let getAllRequest (userRequests: UserRequestsState)  =
        let result = 
            userRequests
            |> Map.toSeq
            |> Seq.map(fun(_, requestState) -> requestState)
            |> Seq.map(convertStates)
            |> Seq.where(fun r -> match r with
                                    | None -> false
                                    | _ -> true)
            |> Seq.map(fun r -> r.Value)
            |> Seq.toList

        Ok result

    let decide (userRequests: UserRequestsState) (user:User) (command: Command) dateProviderService =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                if user = Manager then
                    Error "Unauthorized"
                else
                    let activeUserRequests =
                        userRequests
                        |> Map.toSeq
                        |> Seq.map (fun (_, state) -> state)
                        |> Seq.where (fun state -> state.IsActive)
                        |> Seq.map (fun state -> state.Request)

                    createRequest activeUserRequests request dateProviderService
            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | RefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState dateProviderService
            | EmployeeCancelRequest (_, requestId) ->
                if user = Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    employeeCancelRequest requestState dateProviderService
            | AskCancelRequest (_, requestId) ->
                if user = Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    askCancelRequest requestState dateProviderService
            | RefuseCanceledRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseCanceledRequest requestState
            | ManagerCancelRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                   let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                   managerCancelRequest requestState

            | GetRequestById (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                getRequestById requestState 
            | GetAllRequest (userId) ->
                getAllRequest userRequests 
                