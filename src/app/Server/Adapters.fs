namespace TimeOff

open TimeOff.DtoTypes
open TimeOff.DomainTypes
open System

module Adapters =
    let timeOffRequestAskDtoAdapter (timeOffRequestAskDto: TimeOffRequestAskDto) =
        let timeOffRequest = {
                UserId = timeOffRequestAskDto.UserId
                RequestId = Guid.Empty;
                Start =  timeOffRequestAskDto.Start
                End =  timeOffRequestAskDto.End
            }
        timeOffRequest

