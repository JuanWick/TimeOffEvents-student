namespace TimeOff

open TimeOff.DomainTypes
open TimeOff.CommandHandler

module QueryHandler = 
    

    let getRequestById requestState =
        
        match requestState with
        | PendingValidation request ->
            Ok [RequestCreated request]
        | Validated request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be found"
   
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

    let execute (userRequests: UserRequestsState) (user:User) (query: Query) dateProviderService =
        let relatedUserId = query.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match query with
            | GetRequestById (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                getRequestById requestState 
            | GetAllRequest (userId) ->
                getAllRequest userRequests 
                
