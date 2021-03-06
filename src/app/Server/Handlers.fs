﻿namespace TimeOff

open Microsoft.AspNetCore.Http
open TimeOff.DomainTypes
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open TimeOff.DtoTypes
open TimeOff.Adapters
open System
open Giraffe
open TimeOff.QueryHandler
open TimeOff.CommandHandler
open TimeOff.EventHandler
open EventStorage

module Handlers =

    //Command Handler
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
        task {
            let command = RefuseCanceledRequest (idUser, idRequest)
            let result = handleCommand(eventStore) (user) (command)
            match result with
            | Ok [RequestCancelRefused timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
    }

    let managerCancelRequestHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let command = ManagerCancelRequest (idUser, idRequest)
            let result = handleCommand(eventStore) (user) (command)
            match result with
            | Ok [RequestCanceledByManager timeOffRequest] -> return! json timeOffRequest next ctx
            | Ok _ -> return! Successful.NO_CONTENT next ctx
            | Error message ->
                return! (BAD_REQUEST message) next ctx
    }

    //Request Handler 
    let handleQuery (eventStore: IStore<UserId, RequestEvent>) (user: User) (query: Query) =
        let userId = query.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold evolveUserRequests Map.empty

        let dateProviderService = new DateProvider.DateProviderService()

        let result = execute state user query dateProviderService

        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        result
    
    let requestTimeOffByIdHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int, idRequest : Guid) = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task { 
               
                let query = GetRequestById (idUser, idRequest)
                let result = handleQuery(eventStore) (user) (query)
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
            }

    let requestTimeOffListByUserIdHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int) = 
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task {               
                let query = GetAllRequestByUserId (idUser)
                let result = handleQuery(eventStore) (user) (query)
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
            }

    let requestTimeOffSummaryByUserIdHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int) =
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task {

                let eventStream = eventStore.GetStream(idUser)
                let state = eventStream.ReadAll() |> Seq.fold evolveUserRequests Map.empty

                let dateProviderService = new DateProvider.DateProviderService()
    
                let query = GetSummaryByUserId (idUser)
                let result = getSummaryByUserId (state) (user) (query) dateProviderService

                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
            }
    
    let requestTimeOffHistoryByUserIdHandler (eventStore: IStore<UserId, RequestEvent>) (user:User) (idUser : int) = //TODO
        fun (next : HttpFunc) (ctx : HttpContext) -> 
            task {

                let eventStream = eventStore.GetStream(idUser)
                let state = eventStream.ReadAll() |> Seq.fold getAllUserRequests List.empty

                let dateProviderService = new DateProvider.DateProviderService()
                let query = GetHistoryByUserId (idUser)

                let result = getHistoryByUserId state user query dateProviderService
                
                match result with
                | Ok _ -> return! json result next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx 
            }


