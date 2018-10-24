namespace TimeOff

open HalfDay
open System

module Boundary =
    type Boundary = {
        Date: DateTime
        HalfDay: HalfDay
    }
