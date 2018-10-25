namespace TimeOff

open System

module ICustomDate =

    type ICustomDate = 
       abstract member Now: unit -> DateTime

       abstract member CustomDate: int * int * int -> DateTime