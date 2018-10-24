namespace TimeOff

open TimeOff.Command
open TimeOff.EventStorage
open TimeOff.User
open TimeOff.RequestEvent
open TimeOff.EventHandler
open TimeOff.TimeOffRequest
open System
open TimeOff.RequestState
open TimeOff.UserRequestsState
open TimeOff.IDateProvider


module CommandHandler =

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
        let dateProviderService = new DateProvider.DateProviderService()
        let dateProvider = dateProviderService :>IDateProvider

        if overlapsWithAnyRequest activeUserRequests  request then
            Error "Overlapping request"
        elif request.Start.Date <= dateProvider.Now() then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
        Error "Request cannot be validated"


    let decide (userRequests: UserRequestsState) (command: Command) =
        match command with
        | RequestTimeOff request ->
            let activeUserRequests  =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)

            createRequest activeUserRequests  request

        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState