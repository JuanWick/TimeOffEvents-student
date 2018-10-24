namespace TimeOff

open System

module IDateProvider =

    type IDateProvider = 
       abstract member Now: unit -> DateTime