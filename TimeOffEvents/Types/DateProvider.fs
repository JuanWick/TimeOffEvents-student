namespace TimeOff
open TimeOff.ICustomDate

module DateProvider =

    type DateProviderService() =
    
        interface ICustomDate with
            member this.CustomDate(arg1: int, arg2: int, arg3: int): System.DateTime = 
                System.DateTime(arg1,arg2,arg3)            
            member this.Now(): System.DateTime = 
                System.DateTime.UtcNow
        
        
        