namespace TimeOff

open System
open TimeOff.RequestState

module UserRequestsState =

    type UserRequestsState = Map<Guid, RequestState>