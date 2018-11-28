module TimeOff.Tests

open Expecto
open TimeOff.DomainTypes
open TimeOff.EventHandler
open TimeOff.CommandHandler
open TimeOff.ICustomDate

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

open System
open TimeOff

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
    test "A request is created" {
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
    test "A request is validated" {
      let request = {
        UserId = 1
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest (1, Guid.Empty))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let cancelTests =
  testList "Cancel tests" [
    //test "A request is cancel by user" {
      //let dateProviderService = new DateProvider.DateProviderService()
     // let dateProvider = dateProviderService :>ICustomDate

     // let request = {
      //  UserId = 1
      //  RequestId = Guid.Empty
      //  Start = { Date = dateProvider.CustomDate(2018, 12, 28); HalfDay = AM }
      //  End = { Date = dateProvider.CustomDate(2018, 12, 28); HalfDay = PM } }

     // Given []
     // |> When (CancelRequest (1, Guid(1)))
     // |> Then (Ok [RequestValidated request]) "The request should have been validated"
      // }
      ]