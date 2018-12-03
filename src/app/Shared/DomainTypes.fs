namespace TimeOff

open System

module DomainTypes =

    type UserId = int

    type User =
        | Employee of UserId
        | Manager

    type HalfDay = | AM | PM

    [<CLIMutable>]
    type Boundary = {
        Date: DateTime
        HalfDay: HalfDay
    }

    [<CLIMutable>]
    type TimeOffRequest = {
        UserId: UserId
        RequestId: Guid
        Start: Boundary
        End: Boundary
    }

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused of TimeOffRequest
        | Canceled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Canceled request -> request
            | Refused request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Canceled _ -> false
            | Refused _ -> false

    type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestCanceled of TimeOffRequest with
        member this.Request =
            match this with
            | RequestCreated request -> request
            | RequestValidated request -> request
            | RequestCanceled request -> request
            | RequestRefused request -> request

    type UserRequestsState = Map<Guid, RequestState>

    type Command =
        | RequestTimeOff of TimeOffRequest
        | ValidateRequest of UserId * Guid
        | GetRequestById of UserId * Guid
        | GetAllRequest of UserId
        | RefuseRequest of UserId * Guid 
        | CancelRequest of UserId * Guid with
        member this.UserId =
            match this with
            | RequestTimeOff request -> request.UserId
            | ValidateRequest (userId, _) -> userId
            | GetRequestById (userId, _) -> userId
            | GetAllRequest (userId) -> userId 
            | CancelRequest (userId, _) -> userId
            | RefuseRequest (userId, _) -> userId
     
    //type Query = 
    //    | GetAllRequest of UserId
    //    | GetRequestById of UserId * Guid
    //    member this.UserId =
    //        match this with
    //        | GetRequestById (userId, _) -> userId
    //        | GetAllRequest (userId) -> userId 
    