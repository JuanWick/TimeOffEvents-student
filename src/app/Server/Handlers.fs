namespace TimeOff

open Microsoft.AspNetCore.Http
open TimeOff.DomainTypes
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open TimeOff.DtoTypes
open TimeOff.Adapters
open System
open Giraffe
open TimeOff.CommandHandler
open TimeOff.EventHandler
open EventStorage

module Handlers =

    let handleCommand (eventStore: IStore<UserId, RequestEvent>) (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold evolveUserRequests Map.empty

        let dateProviderService = new DateProvider.DateProviderService()

        // Decide how to handle the command
        let result = decide state user command dateProviderService

        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result

    let requestTimeOffByIdHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task { 
               
                let command = GetRequestById (idUser, idRequest)
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
            }

    let requestTimeOffListHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int) = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task {               
                let command = GetAllRequest (idUser)
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
        }

    let requestTimeOffHandler (eventStore: IStore<UserId, RequestEvent>) (user:User)  =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequestAskDto = ctx.BindJsonAsync<TimeOffRequestAskDto>()

                let timeOffRequest = timeOffRequestAskDtoAdapter(timeOffRequestAskDto)

                let command = RequestTimeOff timeOffRequest
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok [RequestCreated timeOffRequest] -> 
                    return! json timeOffRequest next ctx
                | Ok [RequestValidated timeOffRequest] -> 
                    return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
                | _ ->
                    return! (BAD_REQUEST "Erreur : Cas non géré") next ctx
            }

    let validateRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok [RequestValidated timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let refuseRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) = 
            fun (next : HttpFunc) (ctx : HttpContext) -> 
                task { 
               
                    let command = RefuseRequest (idUser, idRequest)
                    let result = handleCommand(eventStore) (user) (command)
                    match result with
                    | Ok _ -> return! json result next ctx
                    | Error message ->
                        return! (BAD_REQUEST message) next ctx 
                }

    let employeeCancelRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task {
                let command = EmployeeCancelRequest (idUser, idRequest)
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }  

    let askCancelRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
                let command = AskCancelRequest (idUser, idRequest)
                let result = handleCommand(eventStore) (user) (command)
                match result with
                | Ok [RequestAskedCancel timeOffRequest] -> return! json timeOffRequest next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let refuseCanceledRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
        task {//TODO
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = AskCancelRequest (idUser, idRequest)
            let result = handleCommand(eventStore) (user) (command)
            match result with
            | Ok [RequestAskedCancel timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
    }

    let managerCancelRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
        task {//TODO
            let userAndRequestId = ctx.BindQueryString<UserAndRequestId>()
            let command = AskCancelRequest (idUser, idRequest)
            let result = handleCommand(eventStore) (user) (command)
            match result with
            | Ok [RequestAskedCancel timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
    }

    
    
    


