namespace TimeOff
open TimeOff.IDateProvider

module DateProvider =

    type DateProviderService() =
    
        interface IDateProvider with
            member this.Now(): System.DateTime = 
               System.DateTime.UtcNow
