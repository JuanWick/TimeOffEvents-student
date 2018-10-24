namespace TimeOff

open TimeOff.RequestState
open TimeOff.RequestEvent
open TimeOff.UserRequestsState
open TimeOff.TimeOffRequest
open System

module EventHandler =
    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)
   
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
      

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
        Error "Request cannot be validated"