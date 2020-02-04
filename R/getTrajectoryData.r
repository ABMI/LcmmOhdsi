#'get Longitudinal data for trajectory clustering
#'@import dplyr
#'@param connectionDetails
#'@param CDMschema
#'@param oracleTempSchema 
#'@param Resultschema
#'@param cohortTable
#'@param temporalSetting
#'@param demographicSetting
#'@param TargetMeasurementConceptId
#'@param trajectoryByUnit
#'@param valueSelectMethod
#'@param minimumMeasurementCount
#'@param TargetCohortId
#'@export

getTrajectoryData <- function(connectionDetails,
                              CDMschema,
                              oracleTempSchema = NULL,
                              Resultschema,
                              cohortTable,
                              temporalSetting,
                              demographicSetting,
                              TargetMeasurementConceptId,
                              trajectoryByUnit,
                              valueSelectMethod,
                              minimumMeasurementCount,
                              TargetCohortId){
    #1. connection
    connection <- DatabaseConnector::connect(connectionDetails)
    #2. getPLPdata
    TemporalPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = CDMschema,
                                                          oracleTempSchema = oracleTempSchema,
                                                          cohortId = TargetCohortId,
                                                          outcomeIds = TargetCohortId,
                                                          cohortDatabaseSchema = Resultschema,
                                                          cohortTable = cohortTable,
                                                          outcomeDatabaseSchema = Resultschema,
                                                          outcomeTable = cohortTable,
                                                          covariateSettings = temporalSetting)
    demographicPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                             cdmDatabaseSchema = CDMschema,
                                                             oracleTempSchema = oracleTempSchema,
                                                             cohortId = TargetCohortId,
                                                             outcomeIds = TargetCohortId,
                                                             cohortDatabaseSchema = Resultschema,
                                                             cohortTable = cohortTable,
                                                             outcomeDatabaseSchema = Resultschema,
                                                             outcomeTable = cohortTable,
                                                             covariateSettings = demographicSetting)
    ### TemporalPlpData$timeRef
    covariatePlpData <- ff::as.ram(TemporalPlpData$covariates)
    covariateRef <- ff::as.ram(TemporalPlpData$covariateRef)
    timeRef <- ff::as.ram(TemporalPlpData$timeRef)
    subjectRef <- TemporalPlpData$cohorts %>% select(rowId,subjectId)
    covariateData <- covariatePlpData %>%
        left_join(covariateRef%>%select(covariateId,conceptId),by = "covariateId") %>%
        left_join(timeRef%>%select(timeId,startDay),by = "timeId") %>%
        left_join(subjectRef, by = "rowId")
    covariateData <- covariateData %>% select(subjectId,conceptId,covariateValue,startDay)
    getAllMeasure <- covariateData %>% dplyr::mutate(cohortId = TargetCohortId)
    
    getFEV1 <- covariateData %>% filter(conceptId == TargetMeasurementConceptId)
    timeSep <- switch(trajectoryByUnit, "day" =  {1}, "year" = {365.25})
    getFEV1 <- getFEV1 %>% mutate(timeDivTimeUnit = startDay%/%timeSep)
    getFEV1selected <- getFEV1 %>% group_by(subjectId,timeDivTimeUnit) %>% 
        summarise(covariateValue = if_else(valueSelectMethod == 'minimum',min(covariateValue),max(covariateValue)) )
    subjectIdMorethanMinimumCount <- subset(table(getFEV1selected$subjectId),table(getFEV1selected$subjectId)>=minimumMeasurementCount)
    # length(subjectIdMorethan2)
    getFEV1MoreThanMinimimCount  <- getFEV1selected %>% filter(subjectId %in% as.numeric(names(subjectIdMorethanMinimumCount)))
    
    ### AgePlpdata
    ageCovariatePlpData <- ff::as.ram(demographicPlpData$covariates) %>% filter(covariateId == 1002)
    agesubjectRef <- demographicPlpData$cohorts %>% select(rowId,subjectId)
    ageCovariatePlpData <- ageCovariatePlpData %>% left_join(agesubjectRef, by = "rowId")
    ageCovariateData <- ageCovariatePlpData %>% select(subjectId,covariateValue) %>% rename(ageAtIndex = covariateValue)
    # %>% filter(ageAtIndex >=20 & ageAtIndex <60)
    
    ### GenderPlpData
    genderCovariatePlpData <- ff::as.ram(demographicPlpData$covariates) %>% filter(covariateId != 1002)
    
    ### put age and FEV1 together
    FEV1byAgeData <- getFEV1AtLeast2times %>% left_join(ageCovariateData, by = "subjectId") %>% mutate(ageAtMeasure = ageAtIndex + timeYear) %>% as.data.frame()
    
    class(FEV1byAgeData) <- "LongitudinalData"
    
    return(FEV1byAgeData)
}
doLCMM <- function(LCMMsetting){
    getTrajectoryData()
}