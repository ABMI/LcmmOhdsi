# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of LcmmOhdsi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' get temporalPlpData for lcmm
#' @name getTemporalPlpData
#' @param startDay                         should be integer
#' @param endDay                           should be integer
#' @param daysInterval                     should be integer
#' @param connectionDetails
#' @param cdmDatabaseSchema
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortId
#' @param outcomeCohortId
#' @param washoutPeriod                    integer (default = 0)
#' @param firstExposureOnly                logical (default = FALSE)
#' @param removeSubjectsWithPriorOutcome   logical (default = TRUE)
#' @param priorOutcomeLookback             integer (default = 99999)
#' @param requireTimeAtRisk                logical (default = FALSE)
#' @param minTimeAtRisk                    integer (default = 0)
#' @param riskWindowStart                  integer, if NULL, riskWindowStart = startDay
#' @param addExposureDaysToStart           logical (default = FALSE)
#' @param riskWindowEnd                    integer, if NULL, riskWindowEnd = endDay
#' @param addExposureDaysToEnd             logical (default = FALSE)
#' @export
#' 
getTemporalPlpData <- function(startDay,
                               endDay,
                               daysInterval,
                               connectionDetails,
                               cdmDatabaseSchema,
                               resultDatabaseSchema,
                               cohortTable,
                               targetCohortId,
                               outcomeCohortId,
                               washoutPeriod = 0,
                               firstExposureOnly = FALSE,
                               removeSubjectsWithPriorOutcome = TRUE,
                               priorOutcomeLookback = 99999,
                               requireTimeAtRisk = FALSE,
                               minTimeAtRisk = 0,
                               riskWindowStart = NULL,
                               addExposureDaysToStart = FALSE,
                               riskWindowEnd = NULL,
                               addExposureDaysToEnd = FALSE ){
    
    temporalSettings <- FeatureExtraction::createTemporalCovariateSettings(useMeasurementValue = TRUE,
                                                                           temporalStartDays = seq(from = startDay,to =endDay, by = daysInterval),
                                                                           temporalEndDays = seq(from = startDay,to =endDay, by = daysInterval)+daysInterval-1 )
    
    temporalPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                                          cohortDatabaseSchema = resultDatabaseSchema,
                                                          outcomeDatabaseSchema = resultDatabaseSchema,
                                                          cohortTable = cohortTable,
                                                          outcomeTable = cohortTable,
                                                          cohortId = targetCohortId,
                                                          outcomeId = outcomeCohortId,
                                                          #rowIdField = "subject_id",
                                                          covariateSettings = temporalSettings,
                                                          sampleSize = NULL)
    
    if(is.null(riskWindowStart)) rws = startDay else rws = riskWindowStart
    if(is.null(riskWindowEnd)) rwe = endDay else rwe = riskWindowEnd
    
    population <- PatientLevelPrediction::createStudyPopulation(plpData = temporalPlpData,
                                                                outcomeId = outcomeCohortId,
                                                                washoutPeriod = washoutPeriod,
                                                                binary = F,
                                                                firstExposureOnly = firstExposureOnly,
                                                                removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                priorOutcomeLookback = priorOutcomeLookback,
                                                                requireTimeAtRisk = requireTimeAtRisk,
                                                                minTimeAtRisk = minTimeAtRisk,
                                                                riskWindowStart = rws,
                                                                addExposureDaysToStart = addExposureDaysToStart,
                                                                riskWindowEnd = rwe,
                                                                addExposureDaysToEnd = addExposureDaysToEnd)
    result <- list(targetCohortId = targetCohortId,
                   outcomeCohortId = outcomeCohortId,
                   plpData = temporalPlpData,
                   population = population)
    class(result) <- "temporalPlpData"
    return(result)
}

#' get demographic data
#' @name demographicData
#' @importFrom dplyr %>%
#' @param useDemographicsAge               logical, If you want to adjust for your age, this has to be TRUE
#' @param useDemographicsGender            logical, If you want to adjust for your gender, this has to be TRUE
#' @param connectionDetails
#' @param cdmDatabaseSchema
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortId
demographic <- function(useDemographicsAge,
                        useDemographicsGender,
                        connectionDetails,
                        cdmDatabaseSchema,
                        resultDatabaseSchema,
                        cohortTable,
                        targetCohortId){
    demographicSetting <- FeatureExtraction::createCovariateSettings(useDemographicsAge = useDemographicsAge,
                                                                     useDemographicsGender = useDemographicsGender)
    demographicPlpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                                             cohortDatabaseSchema = resultDatabaseSchema,
                                                             outcomeDatabaseSchema = resultDatabaseSchema,
                                                             cohortTable = cohortTable,
                                                             outcomeTable = cohortTable,
                                                             cohortId = targetCohortId,
                                                             outcomeId = targetCohortId,
                                                             covariateSettings = demographicSetting,
                                                             sampleSize =NULL)
    demographicCovRef <- ff::as.ram(demographicPlpData$covariateRef)
    demographic_df <- ff::as.ram(demographicPlpData$covariates) 
    demographic_subjectId <- left_join(demographic_df,demographicCovRef, by = "covariateId") %>%
        left_join(demographicPlpData$cohorts %>% select(rowId,subjectId), by = "rowId")
    demographic <- full_join(demographic_subjectId[grep('gender' , demographic_subjectId$covariateName),] %>% select(subjectId,conceptId),
                             demographic_subjectId[grep('age' , demographic_subjectId$covariateName),] %>% select(subjectId,covariateValue),
                             by = "subjectId" ) 
    names(demographic) <- c("subjectId","genderConceptId","ageInYear")
    
    return(demographic)    
}

#' get targetLongtermPopulation data
#' @name longtermPopulation
#' @importFrom dplyr %>%
#' @param temporalPlpData                  result of getTemporalPlpData (list)
#' @param targetConceptIds                 target long-term followed conceptIds
#' @param minMeasurementCount              integer, minimum count of measurement, defualt = 1
#' @param useDemographicsAge               logical, If you want to adjust for your age, this has to be TRUE
#' @param useDemographicsGender            logical, If you want to adjust for your gender, this has to be TRUE
#' @param connectionDetails                If useDemographicsAge or useDemographicsGender is TRUE, then you should fill
#' @param cdmDatabaseSchema                If useDemographicsAge or useDemographicsGender is TRUE, then you should fill
#' @param resultDatabaseSchema             If useDemographicsAge or useDemographicsGender is TRUE, then you should fill
#' @param cohortTable                      If useDemographicsAge or useDemographicsGender is TRUE, then you should fill
#' @param targetCohortId                   If useDemographicsAge or useDemographicsGender is TRUE, then you should fill
#' @export
longtermPopulation <- function(temporalPlpData,
                               targetConceptIds,
                               minMeasurementCount = 1,
                               useDemographicsAge = FALSE,
                               useDemographicsGender = FALSE,
                               connectionDetails = NULL,
                               cdmDatabaseSchema = NULL,
                               resultDatabaseSchema = NULL,
                               cohortTable = NULL,
                               targetCohortId = NULL){
    # subset covariate Reference of interest
    covariateRef <- ff::as.ram(temporalPlpData$covariateRef)
    targetCovariateRef <- covariateRef[covariateRef$conceptId %in% targetConceptIds,]
    
    # time Reference
    timeRef <- ff::as.ram(temporalPlpData$timeRef)
    
    # subset covariates of interest
    covariates <- ff::as.ram(temporalPlpData$covariates)
    targetCovariates <- covariates[covariates$covariateId %in% targetCovariateRef$covariateId,]
    
    # put conceptId, startDay, endDay
    targetLongtermData <- dplyr::left_join(targetCovariates,targetCovariateRef,by = "covariateId") %>%
        dplyr::select(rowId,conceptId,covariateValue,timeId) %>%
        dplyr::left_join(timeRef, by = "timeId")
    
    # put longtermData with population
    targetLongtermPopulation <- dplyr::left_join(population, targetLongtermData, by = "rowId")
    
    # if targetCohortId = outcomeCohortId, filter targetLongtermPopulation according to survival time
    if(targetCohortId != outcomeCohortId){
        targetLongtermPopulation <- targetLongtermPopulation %>% filter( (survivalTime - 1) >= endDay )
    }
    
    # filter people who have count of measurement less than minMeasurementCount
    if(minMeasurementCount > max(table(targetLongtermPopulation$subjectId)) ) stop('minMeasurementCount is too big, so large that all data is lost')
    LessThanMinMeasurementCount <- names(which(table(targetLongtermPopulation$subjectId) >= minMeasurementCount) )
    targetLongtermPopulation <- targetLongtermPopulation[targetLongtermPopulation$subjectId %in% LessThanMinMeasurementCount,]
    
    # add demographic data
    if(useDemographicsAge|useDemographicsGender){
        demographic <- demographic(useDemographicsAge = useDemographicsAge,
                                   useDemographicsGender = useDemographicsGender,
                                   connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   resultDatabaseSchema = resultDatabaseSchema,
                                   cohortTable = cohortTable,
                                   targetCohortId = targetCohortId)
    }
    if(useDemographicsAge) targetLongtermPopulation <- left_join(targetLongtermPopulation,demographic%>%select(subjectId,ageInYear),by = "subjectId")
    if(useDemographicsGender) targetLongtermPopulation <-  left_join(targetLongtermPopulation,demographic%>%select(subjectId,genderConceptId),by = "subjectId") 
    
    return(targetLongtermPopulation)
}
