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

    let getAllRequestByUserId (userRequests: UserRequestsState) (userId:UserId)  =
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

    // Cumul des congès attribué depuis le début de l'année (on ne prend en compte que les mois terminés)
    let getRequestSumValidatedThisYear (requests:RequestEvent List) dateProviderService = //TODO
        let result = 10

        result

    // Report des congès sur l'année précédente
    let getReportFromLastYear (requests:RequestEvent List) dateProviderService =  //TODO
        let result = 20

        result

    // Cumul des demandes actives entre le début de l'année et la date du jour
    let getRequestDoneThisYear (requests:RequestEvent List) dateProviderService =  //TODO
        let result = 30

        result
    
    // Cumul des demandes prévu entre le lendemain de la demande et la fin de l'annee
    let getRequestWaitingThisYear (requests:RequestEvent List) dateProviderService =  //TODO
        let result = 40

        result

    let getSummaryByUserId (userRequests: UserRequestsState) (user:User) (query: Query) dateProviderService =
        let relatedUserId = query.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
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
    
            // A 
            let ValidatedThisYear = int16(getRequestSumValidatedThisYear result dateProviderService)
            // B
            let ReportedFromLastYear = int16(getReportFromLastYear result dateProviderService)
            // C 
            let RequestsDoneThisYear = int16(getRequestDoneThisYear result dateProviderService)
            // D
            let RequestWaitingThisYear = int16(getRequestWaitingThisYear result dateProviderService)
            //Le solde disponible sur l'annee A + B - (C + D)
            let BalanceThisYear = ValidatedThisYear + ReportedFromLastYear - (RequestsDoneThisYear + RequestWaitingThisYear)

            let summary = {
                UserId = relatedUserId
                Requests = result
                ValidatedThisYear = ValidatedThisYear
                ReportedFromLastYear = ReportedFromLastYear
                RequestsDoneThisYear = RequestsDoneThisYear
                RequestWaitingThisYear = RequestWaitingThisYear
                BalanceThisYear = BalanceThisYear
            }

            Ok summary

    let getHistoryByUserId (userRequests: UserRequestsState) (userId:UserId)  =
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
            | GetAllRequestByUserId (userId) ->
                getAllRequestByUserId userRequests userId
            | GetHistoryByUserId (userId) ->
                getHistoryByUserId userRequests userId
                
    