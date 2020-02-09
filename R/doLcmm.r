#'@export
doLCMM <- function(NumSettingList,
                   data = longitudinalData){
    NumSetting = NumSet[NumSettingList,]
    lcmm_result <- lcmm::hlme(fixed = as.formula(.switchFormula(NumSetting$degreeNum)$`formula`),
                              mixture = as.formula(.switchFormula(NumSetting$degreeNum)$`mixture`),
                              random = ~time,
                              subject = "subjectId",
                              ng = NumSetting$classNum,
                              nwg = TRUE,
                              idiag = FALSE,
                              data = data )
    return(lcmm_result)
}