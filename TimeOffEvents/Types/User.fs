namespace TimeOff

module User = 
    type User =
        | Employee of int
        | Manager

    type UserId = 
        int
