namespace TimeOff

open TimeOff.DomainTypes
open TimeOff.CommandHandler
open TimeOff.ICustomDate

open System


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

    let removeHalfDay (days) (date:DateTime) =
         match date.Hour with
                | 12 -> days - 0.5
                | _ -> days

    // On ne prend pas tout ce qui est strictement inférieur à la date de fin de requestlimit et supérieur/égale au 1er janvier de l'année suivante
    let computeRequestTimeFromOverlaping requestlimit request =

        let startDate =
            match request.Start.Date <= requestlimit.Start.Date with
            | true ->  match request.Start.HalfDay with
                       | AM -> System.DateTime(requestlimit.Start.Date.Year,requestlimit.Start.Date.Month,requestlimit.Start.Date.Day,0,0,0)
                       | PM ->  System.DateTime(requestlimit.Start.Date.Year,requestlimit.Start.Date.Month,requestlimit.Start.Date.Day,12,0,0)
            | false -> match request.Start.HalfDay with
                       | AM -> System.DateTime(request.Start.Date.Year,request.Start.Date.Month,request.Start.Date.Day,0,0,0)
                       | PM ->  System.DateTime(request.Start.Date.Year,request.Start.Date.Month,request.Start.Date.Day,12,0,0)
        
        let endDate =
            match request.End.Date >= requestlimit.End.Date with
            | true -> match request.End.HalfDay with
                       | AM -> System.DateTime(requestlimit.End.Date.Year,requestlimit.End.Date.Month,requestlimit.End.Date.Day,12,0,0)
                       | PM ->  System.DateTime(requestlimit.End.Date.Year,requestlimit.End.Date.Month,requestlimit.End.Date.Day,23,59,59)
            | false -> match request.End.HalfDay with
                       | AM -> System.DateTime(request.End.Date.Year,request.End.Date.Month,request.End.Date.Day,12,0,0)
                       | PM ->  System.DateTime(request.End.Date.Year,request.End.Date.Month,request.End.Date.Day,23,59,59)

        let result = 
            endDate.Subtract startDate
            |> fun (r) -> r.Days + 1
            |> fun (r) -> removeHalfDay(float(r)) startDate
            |> fun (r) -> removeHalfDay(r) endDate
        result

    // Cumul des congès attribué depuis le début de l'année (on ne prend en compte que les mois terminés)
    let getRequestSumValidatedThisYear (requests:RequestEvent List) dateProviderService = 
        
        (* 
        On définit l'intervalle dans lequel l'on souhaite recenser les request :
        - il commence au 1er janvier de l'année en cours.
        - il se termine au dernier mois terminé à la date de la demande.
        *)
        let dateProvider = dateProviderService :>ICustomDate
        let now = dateProvider.Now()
        let lastDay = DateTime.DaysInMonth(now.Year, now.Month - 1)
        let requestTest = {
            UserId = 0
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(now.Year, 1, 1); HalfDay = AM } //Année du jour
            End = { Date = DateTime(now.Year,now.Month - 1, lastDay); HalfDay = PM } //Date du jour
        }

        //On ne prend que les request Validates
        let result = 
            requests
            |> List.toSeq
            |> Seq.where(fun r -> match r with
                                    | RequestValidated _-> true
                                    | _ -> false)
            |> Seq.where(fun r -> match overlapsWith requestTest r.Request with
                                    | true -> true
                                    | false -> false)
            |> Seq.map(fun(r) -> computeRequestTimeFromOverlaping requestTest r.Request)
            |> Seq.reduce(+)

        float(result)

    // Report des congès sur l'année précédente
    let getReportFromLastYear (requests:RequestEvent List) dateProviderService =  
        // Un employé peut faire une demande de congé d'une durée de trois mois dès son arrivé
        let nbDayByYear = 60.0

        // On récupère le nombre de jour de congès pris sur l'année précédente
        let dateProvider = dateProviderService :>ICustomDate
        let now = dateProvider.Now()
        let requestTest = {
            UserId = 0
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(now.Year-1, 1, 1); HalfDay = AM } //Année du jour
            End = { Date = DateTime(now.Year-1,12, 31); HalfDay = PM } //Date du jour
        }

        //On ne prend que les request Validates
        let result = 
            requests
            |> List.toSeq
            |> Seq.where(fun r -> match r with
                                    | RequestValidated _-> true
                                    | _ -> false)
            |> Seq.where(fun r -> match overlapsWith requestTest r.Request with
                                    | true -> true
                                    | false -> false)
            |> Seq.map(fun(r) -> computeRequestTimeFromOverlaping requestTest r.Request)
            |> Seq.reduce(+)

        float(nbDayByYear - result)

    // Cumul des demandes actives entre le début de l'année et la date du jour
    let getRequestDoneThisYear (requests:RequestEvent List) dateProviderService =  //TODO
        (* 
        On définit l'intervalle dans lequel l'on souhaite recenser les request :
        - il commence au 1er janvier de l'année en cours.
        - il se termine au dernier mois terminé à la date de la demande.
        *)
        let dateProvider = dateProviderService :>ICustomDate
        let now = dateProvider.Now()
        let lastDay = DateTime.DaysInMonth(now.Year, now.Month - 1)
        let requestTest = {
            UserId = 0
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(now.Year, 1, 1); HalfDay = AM } //Année du jour
            End = { Date = now; HalfDay = PM } //Date du jour
        }

        //On ne prend que les request Validates
        let result = 
            requests
            |> List.toSeq
            |> Seq.where(fun r -> match r with
                                    | RequestCreated _ -> true
                                    | RequestValidated _-> true
                                    | RequestAskedCancel _ -> true
                                    | RequestCanceledByManager _ -> true
                                    | _ -> false)
            |> Seq.where(fun r -> match overlapsWith requestTest r.Request with
                                    | true -> true
                                    | false -> false)
            |> Seq.map(fun(r) -> computeRequestTimeFromOverlaping requestTest r.Request)
            |> Seq.reduce(+)

        float(result)
    
    // Cumul des demandes prévu entre le lendemain de la demande et la fin de l'annee
    let getRequestWaitingThisYear (requests:RequestEvent List) dateProviderService =  //TODO
        (* 
        On définit l'intervalle dans lequel l'on souhaite recenser les request :
        - il commence au 1er janvier de l'année en cours.
        - il se termine au dernier mois terminé à la date de la demande.
        *)
        let dateProvider = dateProviderService :>ICustomDate
        let now = dateProvider.Now()
        let lastDay = DateTime.DaysInMonth(now.Year, now.Month - 1)
        let requestTest = {
            UserId = 0
            RequestId = Guid.NewGuid()
            Start = { Date = now.AddDays(1.0); HalfDay = AM } //Année du jour
            End = { Date = DateTime(now.Year, 12, 31); HalfDay = PM } //Date du jour
        }

        //On ne prend que les request Validates
        let result = 
            requests
            |> List.toSeq
            |> Seq.where(fun r -> match r with
                                    | RequestCreated _ -> true
                                    | RequestValidated _-> true
                                    | RequestAskedCancel _ -> true
                                    | RequestCanceledByManager _ -> true
                                    | _ -> false)
            |> Seq.where(fun r -> match overlapsWith requestTest r.Request with
                                    | true -> true
                                    | false -> false)
            |> Seq.map(fun(r) -> computeRequestTimeFromOverlaping requestTest r.Request)
            |> Seq.reduce(+)

        float(result)

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
            let ValidatedThisYear = getRequestSumValidatedThisYear result dateProviderService
            // B
            let ReportedFromLastYear = getReportFromLastYear result dateProviderService
            // C 
            let RequestsDoneThisYear = getRequestDoneThisYear result dateProviderService
            // D
            let RequestWaitingThisYear = getRequestWaitingThisYear result dateProviderService
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
                
    