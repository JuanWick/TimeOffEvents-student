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
        | CanceledByEmployee of TimeOffRequest
        | AskCanceled of TimeOffRequest
        | CancelRefused of TimeOffRequest
        | CanceledByManager of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Refused request -> request
            | CanceledByEmployee request -> request
            | AskCanceled request -> request
            | CancelRefused request -> request
            | CanceledByManager request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | Validated _ -> true
            | Refused _ -> false
            | CanceledByEmployee _ -> false
            | AskCanceled _ -> true
            | CancelRefused _ -> true
            | CanceledByManager _ -> false

    type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestCanceledByEmployee of TimeOffRequest 
    | RequestAskedCancel of TimeOffRequest
    | RequestCancelRefused of TimeOffRequest
    | RequestCanceledByManager of TimeOffRequest
    with
        member this.Request =
            match this with
            | RequestCreated request -> request
            | RequestValidated request -> request
            | RequestRefused request -> request
            | RequestCanceledByEmployee request -> request
            | RequestAskedCancel request -> request
            | RequestCancelRefused request -> request
            | RequestCanceledByManager request -> request

    type UserRequestsState = Map<Guid, RequestState>
    type UserRequestsHistory = List<RequestState>

    type Command =
        | RequestTimeOff of TimeOffRequest
        | ValidateRequest of UserId * Guid
        | RefuseRequest of UserId * Guid
        | EmployeeCancelRequest of UserId * Guid 
        | AskCancelRequest of UserId * Guid
        | RefuseCanceledRequest of UserId * Guid 
        | ManagerCancelRequest of UserId * Guid with
        member this.UserId =
            match this with
            | RequestTimeOff request -> request.UserId
            | ValidateRequest (userId, _) -> userId
            | RefuseRequest (userId, _) -> userId
            | EmployeeCancelRequest (userId, _) -> userId
            | AskCancelRequest (userId, _) -> userId
            | RefuseCanceledRequest (userId, _) -> userId
            | ManagerCancelRequest (userId, _) -> userId
     
    type Query = 
        | GetAllRequestByUserId of UserId
        | GetRequestById of UserId * Guid
        | GetSummaryByUserId of UserId
        | GetHistoryByUserId of UserId
        member this.UserId =
            match this with
            | GetRequestById (userId, _) -> userId
            | GetAllRequestByUserId (userId) -> userId
            | GetSummaryByUserId (userId) -> userId
            | GetHistoryByUserId (userId) -> userId

    type UserRequestSummary = {
        UserId: UserId
        Requests: RequestEvent List
        ValidatedThisYear: float
        ReportedFromLastYear: float
        RequestsDoneThisYear: float
        RequestWaitingThisYear: float
        BalanceThisYear: float
    }
    