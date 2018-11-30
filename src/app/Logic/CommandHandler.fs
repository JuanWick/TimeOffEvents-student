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

    let createRequest activeUserRequests  request dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate

        if overlapsWithAnyRequest activeUserRequests  request then
            Error "Overlapping request"
        elif request.Start.Date <= dateProvider.Now() then
            Error "The request starts in the past"
        else
            let timeOffRequestToSave = {
                UserId = request.UserId
                RequestId = Guid.NewGuid();
                Start =  request.Start
                End =  request.End
            }
            Ok [RequestCreated timeOffRequestToSave]

    let cancelRequest request dateProviderService =
        let dateProviderService = dateProviderService
        let dateProvider = dateProviderService :>ICustomDate

        if request.Start.Date <= dateProvider.Now() then
            Error "The request starts in the past"
        else
            Ok [RequestCanceled request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let getRequestById requestState =
        
        match requestState with
        | PendingValidation request ->
            Ok [RequestCreated request]
        | Validated request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be found"
    let convertStates(requestState:RequestState)  =
        match requestState with 
        | PendingValidation request ->
             Some (RequestCreated request)
        | Validated request ->
             Some (RequestValidated request)
        | Canceled request ->
             Some (RequestCanceled request)
        | NotCreated -> None

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
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelRequest requestState.Request dateProviderService
            | GetRequestById (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                getRequestById requestState 
            | GetAllRequest (userId) ->
                getAllRequest userRequests 
                