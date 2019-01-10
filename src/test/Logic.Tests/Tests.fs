module TimeOff.Tests

open Expecto
open TimeOff.DomainTypes
open TimeOff.EventHandler
open TimeOff.CommandHandler
open TimeOff.QueryHandler
open TimeOff.ICustomDate
open System
open TimeOff

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty

    let dateProviderService = new DateProvider.DateTestProviderService()

    let result = decide userRequestsState user command dateProviderService
    Expect.equal result expected message

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (overlapsWith request request) "A request should overlap with istself"
    }

    test "Date : R1!=R2" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (overlapsWith request1 request2) "The requests don't overlap"
    }
    test "Date : End_R1=Begin_R2 | HalfDay : End_R1=PM ; Begin_R2=PM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }

      Expect.isTrue (overlapsWith request1 request2) "The end of the first request should overlap the second"
    }
    test "Date : End_R1=Begin_R2 | HalfDay : End_R1=AM ; Begin_R2=PM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = AM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }
      Expect.isFalse (overlapsWith request1 request2) "The end of the first request should not overlap the second"
    }
    test "Date : End_R1=Begin_R2 | HalfDay : End_R1=PM ; Begin_R2=AM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }
      Expect.isTrue (overlapsWith request1 request2) "The end of the first request should overlap the second"
    }
    test "Date : Begin_R1=End_R2 | HalfDay : Begin_R1=PM ; End_R2=PM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 6); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      Expect.isTrue (overlapsWith request1 request2) "The beginning of the first request should overlap the second"
    }
    test "Date : Begin_R1=End_R2 | HalfDay : Begin_R1=AM ; End_R2=PM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 6); HalfDay = AM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      Expect.isTrue (overlapsWith request1 request2) "The beginning of the first request should overlap the second"
    }
    test "Date : Begin_R1=End_R2 | HalfDay : Begin_R1=PM ; End_R2=AM" {
      let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 3); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 6); HalfDay = PM }
      }

      let request2 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = AM }
      }

      Expect.isFalse (overlapsWith request1 request2) "The beginning of the first request should not overlap the second"
    }
    test "Date : R1 inside R2 | HalfDay : R1 = R2" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 6); HalfDay = PM }
        }

        Expect.isTrue (overlapsWith request1 request2) "The complete first request should overlap the second"
    }
    test "Multiple check overlaps with conflict" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 3); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 4); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 6); HalfDay = PM }
        }

        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 12); HalfDay = AM }
            End = { Date = DateTime(2019, 4, 6); HalfDay = PM }
        }

         let newRequest = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 13); HalfDay = AM }
            End = { Date = DateTime(2019, 3, 14); HalfDay = PM }
        }
        let oldRequests = 
            seq{
            yield request1
            yield request2
            yield request3
            }
        Expect.isTrue (overlapsWithAnyRequest oldRequests newRequest) "The third old request should overlap the new"
    }
    test "Multiple check overlaps without conflict" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 3); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 4); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 6); HalfDay = PM }
        }

        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 3, 12); HalfDay = AM }
            End = { Date = DateTime(2019, 4, 6); HalfDay = PM }
        }

         let newRequest = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 13); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 14); HalfDay = PM }
        }
        let oldRequests = 
            seq{
            yield request1
            yield request2
            yield request3
            }
        Expect.isFalse (overlapsWithAnyRequest oldRequests newRequest) "none old request should overlap the new"
    }
    test "Empty Multiple check overlaps without conflict" {
        let newRequest = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 13); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 14); HalfDay = PM }
        }
        let oldRequests = Seq.empty
        Expect.isFalse (overlapsWithAnyRequest oldRequests newRequest) "none old request should overlap the new"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    //user tests
    test "A request is created by an employee" {
      
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
    test "A request cannot be created for another employee" {
      
      let request = {
        UserId = 2
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> When (RequestTimeOff request)
      |> Then (Error "Unauthorized") "The request should not have been created"
    }
    test "A request can't be created by an manager" {
      
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Manager)
      |> When (RequestTimeOff request)
      |> Then (Error "Unauthorized") "The request should not have been created"
    }
    //rules tests
    test "A request in the past is not created" {
      let dateProviderService = new DateProvider.DateTestProviderService()
      let dateProvider = dateProviderService :>ICustomDate

      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = dateProvider.CustomDate(2018, 11 ,1); HalfDay = AM }
        End = { Date = dateProvider.CustomDate(2018, 11, 3); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created"
    }
    test "A request for today is not created" {
      let dateProviderService = new DateProvider.DateTestProviderService()
      let dateProvider = dateProviderService :>ICustomDate

      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = dateProvider.CustomDate(2018, 11 ,4); HalfDay = AM }
        End = { Date = dateProvider.CustomDate(2018, 11, 6); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee 1)
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created"
    }
    test "A request in conflit is not created" {
      let dateProviderService = new DateProvider.DateTestProviderService()
      let dateProvider = dateProviderService :>ICustomDate      
     
      let oldRequest = RequestValidated {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = dateProvider.CustomDate(2018, 11 ,1); HalfDay = AM }
        End = { Date = dateProvider.CustomDate(2018, 11, 3); HalfDay = PM } }


      let newRequest = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = dateProvider.CustomDate(2018, 11 ,1); HalfDay = AM }
        End = { Date = dateProvider.CustomDate(2018, 11 ,1); HalfDay = PM }
        }

      Given [oldRequest]
      |> ConnectedAs (Employee 1)
      |> When (RequestTimeOff newRequest)
      |> Then (Error "Overlapping request") "The request should not have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    //user tests
    test "A request is validated by a manager" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2020, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
      }

      let guid1 = request.RequestId

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest (1, guid1))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
    test "A request can't be validated by an employee" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2020, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
      }

      let guid1 = request.RequestId

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee 1)
      |> When (ValidateRequest (1, guid1))
      |> Then (Error "Unauthorized") "The request should have been validated"
    }
    //rules tests
    //state tests
    test "An Refused request can't be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }
    test "An CanceledByEmployee request can't be validated by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }
    test "An CancelRefused request can't be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }
    test "An CanceledByManager request can't be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByManager request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }
    test "An AskCanceled request can't be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }
    test "An Validated request can be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Manager)
        |> When (ValidateRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been validated by manager"
    }

    test "An PendingValidation request can be validated by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Ok [RequestRefused request1]) "The request should have been validated by manager"
    }
  ]

[<Tests>]
let refuseTests =
  testList "Refuse tests" [
    //user tests
    test "A waiting request can be refused by manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Ok [RequestRefused request1]) "The request should have been refused"
    }
    test "A waiting request can't be refused by an employee" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 1)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should not have been refused"
    }
    //rules tests
    test "A request in the past can't be refused by manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2000, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2000, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "The request starts in the past") "The request should not have been refused"
    }
    test "A request for today can't be refused by manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 5); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 5); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "The request starts in the past") "The request should not have been refused"
    }
    test "A request in the futur can be refused by manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2028, 11, 5); HalfDay = AM }
            End = { Date = DateTime(2028, 11, 5); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Ok [RequestRefused request1]) "The request should have been refused"
    }
    //state tests
    test "An Refused request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }
    test "An CanceledByEmployee request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }
    test "An CancelRefused request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }
    test "An CanceledByManager request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByManager request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }
    test "An AskCanceled request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }
    test "An Validated request can be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been refused by manager"
    }

    test "An PendingValidation request can be refuse by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Manager)
        |> When (RefuseRequest (1, guid1))
        |> Then (Ok [RequestRefused request1]) "The request should have been refused by manager"
    }
  ]

[<Tests>]
let cancelAsEmployeeTests =
  testList "Cancel tests" [
    //user tests
    test "A waiting request can be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByEmployee request1]) "The request should have been canceled"
    }
    test "A waiting request can't be cancel by another employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 2)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should have been canceled"
    }  
    test "A waiting request can't be cancel by manager" {
    let request1 = {
        UserId = 1
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
    }
    let guid1 = request1.RequestId
       
    Given [RequestValidated  request1]
    |> ConnectedAs (Manager)
    |> When (EmployeeCancelRequest (1, guid1))
    |> Then (Error "Unauthorized") "The request should not have been canceled"
    }
    //rules tests
    test "A waiting request can be cancel by employee before append" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByEmployee request1]) "The request should have been canceled"
    }
    test "A waiting request can't be cancel by employee the day it append" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 5); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 5); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "The request starts in the past") "The request should have been canceled"
    }
    test "A waiting request can't be cancel by employee after it append" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 11, 5); HalfDay = AM }
            End = { Date = DateTime(2010, 11, 5); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated  request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "The request starts in the past") "The request should have been canceled"
    }
    //state tests
    test "An Refused request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CanceledByEmployee request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CancelRefused request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CanceledByManager request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByManager request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An AskCanceled request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }

    test "An PendingValidation request can be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByEmployee request1]) "The request should have been canceled by employee"
    }
    test "An Validated request can be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Employee 1)
        |> When (EmployeeCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByEmployee request1]) "The request should have been canceled by employee"
    }
  ]

[<Tests>]
let askCanceledTests =
  testList "AskCanceled tests" [
    //user tests
    test "A Validated request can be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Ok [RequestAskedCancel request1]) "The request should have been asked cancel"
    }
    test "A Validated request can't be asked to be cancel by another employee" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Employee 2)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should not have been asked to be cancel"
    }
    test "A Validated request can't be asked to be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Manager)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should not have been asked to be cancel"
    }
    //rules tests
    test "A Validated request can't be asked to be cancel by employee before it append" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "The request starts in the futur") "The request should have been asked cancel"
    }
    test "A Validated request can be asked to be cancel by employee the day it append" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 5); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 5); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Ok [RequestAskedCancel request1]) "The request should have been asked cancel"
    }
    test "A Validated request can be asked to be cancel by employee after it append" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2000, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2000, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated  request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Ok [RequestAskedCancel request1]) "The request should have been asked cancel"
    }
    //state tests
    test "An PendingValidation request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An Refused request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CanceledByEmployee request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CancelRefused request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An CanceledByManager request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByManager request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }
    test "An AskCanceled request can't be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been ask canceled"
    }

    test "An Validated request can be asked to be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2010, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2010, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Employee 1)
        |> When (AskCancelRequest (1, guid1))
        |> Then (Ok [RequestAskedCancel request1]) "The request should have been ask to be cancel"
    }
  ]

[<Tests>]
let refuseCancelTests =
  testList "CancelRefused tests" [
    //user tests
    test "An AskCanceled request can be cancelRefused by an employee" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Employee 1)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should not have been cancel"
    }
    test "An AskCanceled request can be cancelRefused by a manager" {
        let request1 = {
            UserId = 1
            RequestId =Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Ok [RequestCancelRefused request1]) "The request should not have been cancel"
    }
    //state tests
    test "An PendingValidation request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should have been canceled"
    }
    test "An Validated request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should have been canceled"
    }
    test "An Refused request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been canceled"
    }
    test "An CanceledByEmployee request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been canceled"
    }
    test "An CancelRefused request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should have been canceled"
    }
    test "An CanceledByManager request can't be cancelRefused by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByManager request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should have been canceled"
    }

    test "An AskCanceled request can be cancelrefused by a manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (RefuseCanceledRequest (1, guid1))
        |> Then (Ok [RequestCancelRefused request1]) "The request should have been cancel refused"
    }
  ]

[<Tests>]
let cancelAsManagerTests =
  testList "Manager Cancel tests" [
    //user tests
    test "An AskCanceled request can be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByManager request1]) "The request should have been canceled"
    }
    test "An AskCanceled request can't be cancel by employee" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Employee 1)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Error "Unauthorized") "The request should have been canceled"
    }
    //state tests
    test "An AskCanceled request is cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestAskedCancel  request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByManager request1]) "The request should have been canceled"
    }
    test "An CancelRefused request is cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCancelRefused request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByManager request1]) "The request should have been canceled"
    }
    test "An Validated request is cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestValidated request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByManager request1]) "The request should have been canceled"
    }
    test "An PendingValidation request is cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCreated request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Ok [RequestCanceledByManager request1]) "The request should have been canceled"
    }
    test "An Refused request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestRefused request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been canceled"
    }
    test "An CanceledByEmployee request can't be cancel by manager" {
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2020, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2020, 10, 1); HalfDay = PM }
        }
        let guid1 = request1.RequestId
       
        Given [RequestCanceledByEmployee request1]
        |> ConnectedAs (Manager)
        |> When (ManagerCancelRequest (1, guid1))
        |> Then (Error "Invalid state for action") "The request should not have been canceled"
    } 
  ]

[<Tests>]
let summaryTests =
  testList "Summary tests" [
    //getRequestSumValidatedThisYear
    test "Should return the sum of 2 validated request in the same year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        }

         let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 7, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 7, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2
        let list = [requestEvent1; requestEvent2]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018
        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 2.5 "Should return 2.5"
    }
    test "Should return 0 when asked the sum of none validated request in the same year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        }

         let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 7, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 7, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestRefused  request1
        let requestEvent2 = RequestRefused  request2
        let list = [requestEvent1; requestEvent2]

        let dateProviderService = new DateProvider.DateTestProviderService()
        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0"
    }
    test "Should return 0 when asked the sum of empty request in the same year" {
        // AM matin; PM après-midi
        let list = []

        let dateProviderService = new DateProvider.DateTestProviderService()
        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0"
    }  
    test "Should not take invalid request status when asked the sum of none validated request in the same year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
        }

         let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 7, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 7, 3); HalfDay = AM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 7, 6); HalfDay = AM }
            End = { Date = DateTime(2018, 7, 10); HalfDay = AM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestRefused  request3
        let requestEvent4 = RequestCanceledByEmployee  request3
        let requestEvent5 = RequestAskedCancel  request3
        let requestEvent6 = RequestCancelRefused  request3
        let requestEvent7 = RequestCanceledByManager  request3

        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4; requestEvent5; requestEvent6; requestEvent7]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018


        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 7.5 "Should return 7.5"
    }
    test "Should return only the sum of 2 validated request in the same year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
        }

         let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 7, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 7, 3); HalfDay = AM }
        }

        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 7, 5); HalfDay = AM }
            End = { Date = DateTime(2017, 7, 10); HalfDay = AM }
        }

        let request4 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2019, 7, 5); HalfDay = AM }
            End = { Date = DateTime(2019, 7, 10); HalfDay = AM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestValidated  request3
        let requestEvent4 = RequestValidated  request4

        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 7.5 "Should return 7.5"
    }
    test "Should properly truncate requests in the same year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 10, 4); HalfDay = AM }
            End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }

         let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 12, 20); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 1); HalfDay = PM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2

        let list = [requestEvent1; requestEvent2]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018


        let result = getRequestSumValidatedThisYear list dateProviderService

        Expect.equal result 2.0 "Should return 2"
    }

    //getReportFromLastYear
    test "Should only report balance of validated request of the last year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 11, 4); HalfDay = AM }
            End = { Date = DateTime(2017, 11, 4); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2017, 1, 1); HalfDay = PM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 1); HalfDay = AM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestValidated  request3

        let list = [requestEvent1; requestEvent2; requestEvent3]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getReportFromLastYear list dateProviderService

        Expect.equal result 58.0 "Should return 58"
    }
    test "Should return 60 when report balance of empty validated request of the last year" {
        // AM matin; PM après-midi
        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 1); HalfDay = AM }
        }

        let requestEvent3 = RequestValidated  request3

        let list = [requestEvent3]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getReportFromLastYear list dateProviderService

        Expect.equal result 60.0 "Should return 60"
    }
    test "Should return 60 when report balance of the last year and there isn't" {
        // AM matin; PM après-midi

        let list = []

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getReportFromLastYear list dateProviderService

        Expect.equal result 60.0 "Should return 60"
    }
    test "Should return negative balance when report balance of validated request of the last year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 3, 1); HalfDay = AM }
            End = { Date = DateTime(2017, 3, 25); HalfDay = AM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 2, 1); HalfDay = AM }
            End = { Date = DateTime(2017, 2, 25); HalfDay = PM }
        }

        let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2017, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2017, 1, 25); HalfDay = PM }
        }

        let requestEvent1 = RequestValidated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestValidated  request3

        let list = [requestEvent1; requestEvent2; requestEvent3]

        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getReportFromLastYear list dateProviderService

        Expect.equal result -14.5 "Should return -14.5"
    }

    //getRequestDoneThisYear
    test "Should report balance of active request from the begining of the year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 4); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 5); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request4 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request5 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request6 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request7 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request8 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestCreated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestRefused  request3
        let requestEvent4 = RequestCanceledByEmployee  request4
        let requestEvent5 = RequestAskedCancel  request5
        let requestEvent6 = RequestCancelRefused  request6
        let requestEvent7 = RequestCanceledByManager  request7
        let requestEvent8 = RequestValidated  request8
       

        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4; requestEvent5; requestEvent6; requestEvent7; requestEvent8]
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestDoneThisYear list dateProviderService

        Expect.equal result 6.5 "Should return 6.5"
    }
    test "Should return 0 when report balance of empty active request from the begining of the year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 11, 4); HalfDay = AM }
            End = { Date = DateTime(2018, 11, 5); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request4 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request5 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request6 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request7 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 6, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 6, 2); HalfDay = AM }
        }

        let request8 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestCanceledByEmployee  request1
        let requestEvent2 = RequestCanceledByEmployee  request2
        let requestEvent3 = RequestCanceledByEmployee  request3
        let requestEvent4 = RequestCanceledByEmployee  request4
        let requestEvent5 = RequestCanceledByEmployee  request5
        let requestEvent6 = RequestCanceledByEmployee  request6
        let requestEvent7 = RequestCanceledByEmployee  request7
        let requestEvent8 = RequestCanceledByEmployee  request8
       
        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4; requestEvent5; requestEvent6; requestEvent7; requestEvent8]
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestDoneThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0.0"
    }
    test "Should return 0 when report balance of none active request from the begining of the year" {
        // AM matin; PM après-midi
        let list = []
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestDoneThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0.0"
    }

    //getRequestWaitingThisYear
    test "Should report balance of active request from today to the end of the year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 4); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 5); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let request4 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let request5 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let request6 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let request7 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 12, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        }

        let request8 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 01, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 01, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestCreated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestRefused  request3
        let requestEvent4 = RequestCanceledByEmployee  request4
        let requestEvent5 = RequestAskedCancel  request5
        let requestEvent6 = RequestCancelRefused  request6
        let requestEvent7 = RequestCanceledByManager  request7
        let requestEvent8 = RequestValidated  request8

        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4; requestEvent5; requestEvent6; requestEvent7; requestEvent8]
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestWaitingThisYear list dateProviderService

        Expect.equal result 6.5 "Should return 6.5"
    }
    test "Should return 0 when report balance of empty active request from today to the end of the year" {
        // AM matin; PM après-midi
        let request1 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 4); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 5); HalfDay = PM }
        }

        let request2 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

         let request3 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let request4 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let request5 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let request6 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let request7 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let request8 = {
            UserId = 1
            RequestId = Guid.NewGuid()
            Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
            End = { Date = DateTime(2018, 1, 2); HalfDay = AM }
        }

        let requestEvent1 = RequestCreated  request1
        let requestEvent2 = RequestValidated  request2
        let requestEvent3 = RequestRefused  request3
        let requestEvent4 = RequestCanceledByEmployee  request4
        let requestEvent5 = RequestAskedCancel  request5
        let requestEvent6 = RequestCancelRefused  request6
        let requestEvent7 = RequestCanceledByManager  request7
        let requestEvent8 = RequestValidated  request8

        let list = [requestEvent1; requestEvent2; requestEvent3; requestEvent4; requestEvent5; requestEvent6; requestEvent7; requestEvent8]
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestWaitingThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0.0"
    }
    test "Should return 0 when report balance of none active request from today to the end of the year" {
        // AM matin; PM après-midi
        let list = []
        let dateProviderService = new DateProvider.DateTestProviderService() //5/11/2018

        let result = getRequestWaitingThisYear list dateProviderService

        Expect.equal result 0.0 "Should return 0.0"
    }

  ]