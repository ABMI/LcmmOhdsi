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
#'@param trajectoryBy
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
                              trajectoryBy,
                              valueSelectMethod,
                              minimumMeasurementCount,
                              TargetCohortId){
    #1. connection
    connection <- DatabaseConnector::connect(connectionDetails)
    #2. getPLPdata
    ParallelLogger::logInfo("get temporal plp data")
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
    ParallelLogger::logInfo("get demographic plp data")
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
    
    getTargetMeasure <- covariateData %>% filter(conceptId == TargetMeasurementConceptId)
    timeSep <- switch(trajectoryByUnit, "day" =  {1}, "year" = {365.25})
    getTargetMeasure <- getTargetMeasure %>% mutate(timeDivTimeUnit = startDay%/%timeSep)
    getTargetMeasureFilter <- getTargetMeasure %>% group_by(subjectId,timeDivTimeUnit) %>% 
        summarise(covariateValue = if_else(valueSelectMethod == 'minimum',min(covariateValue),max(covariateValue)) )
    subjectIdMorethanMinimumCount <- subset(table(getTargetMeasureFilter$subjectId),table(getTargetMeasureFilter$subjectId)>=minimumMeasurementCount)
    
    getTargetMeasureMoreThanMinCount <- getTargetMeasureFilter %>% filter(subjectId %in% as.numeric(names(subjectIdMorethanMinimumCount)))
    
    ### AgePlpdata
    ageCovariatePlpData <- ff::as.ram(demographicPlpData$covariates) %>% filter(covariateId == 1002)
    demographicSubjectRef <- demographicPlpData$cohorts %>% select(rowId,subjectId)
    ageCovariatePlpData <- ageCovariatePlpData %>% left_join(demographicSubjectRef, by = "rowId")
    ageCovariateData <- ageCovariatePlpData %>% select(subjectId,covariateValue) %>% rename(ageAtIndex = covariateValue)
    # %>% filter(ageAtIndex >=20 & ageAtIndex <60)
    
    ### GenderPlpData
    genderCovariatePlpData <- ff::as.ram(demographicPlpData$covariates) %>% filter(covariateId != 1002)
    
    ### put age and FEV1 together
    measurementWithDemographicData <- getTargetMeasureMoreThanMinCount %>% left_join(ageCovariateData, by = "subjectId") %>% as.data.frame()

    if(nrow(genderCovariatePlpData)!=0){
        genderCovariatePlpData <- genderCovariatePlpData %>% left_join(ff::as.ram(demographicPlpData$covariateRef)%>%select(covariateId,conceptId),by = "covariateId")
        genderCovariatePlpData <- genderCovariatePlpData %>% left_join(demographicSubjectRef, by = "rowId")
        genderCovariateData <- genderCovariatePlpData %>% select(subjectId,conceptId) %>% rename(genderConceptId = conceptId)
        measurementWithDemographicData <- measurementWithDemographicData %>% left_join(genderCovariateData, by = "subjectId") %>% as.data.frame()
    }
    
    if(trajectoryBy == 'age' & trajectoryByUnit == 'year'){
        measurementWithDemographicData <- measurementWithDemographicData %>%
            mutate(ageAtMeasure = ageAtIndex + timeDivTimeUnit) 
    }
    
    ParallelLogger::logInfo("longitudinal data were ready")
    # class(measurementWithDemographicData) <- "LongitudinalData"
    longitudinalData <- measurementWithDemographicData
    
    if(trajectoryBy == 'age'){
        longitudinalData$time = longitudinalData$ageAtMeasure
    } else if (trajectoryBy == 'FU'){
        longitudinalData$time = longitudinalData$timeDivTimeUnit
    }
    
    return(longitudinalData)
}