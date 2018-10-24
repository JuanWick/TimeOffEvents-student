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
open TimeOff.Boundary
open TimeOff.HalfDay


module CommandHandler =
    let overlapsWith request1 request2 =
        false //TODO: write a function that checks if 2 requests overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        false //TODO: write this function using overlapsWith

    let createRequest activeUserRequests  request =
        if overlapsWithAnyRequest activeUserRequests  request then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

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