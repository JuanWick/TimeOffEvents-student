namespace TimeOff

open TimeOff.DtoTypes
open TimeOff.DomainTypes
open System

module Adapters =
    let timeOffRequestAskDtoAdapter (timeOffRequestAskDto: TimeOffRequestAskDto) =
   
        let timeOffRequestToSave = {
            UserId = timeOffRequestAskDto.UserId
            RequestId = Guid.NewGuid();
            Start =  timeOffRequestAskDto.Start
            End =  timeOffRequestAskDto.End
        }

        timeOffRequestToSave

