#'setting for LCMM 
#'@param targetCohortId           Cohort Definition Id
#'@param measurementConceptId     measurement concept id interested in
#'@param trajectoryDuration       vector; from time window start to window end; 0:365 
#'@param trajectoryBy             character; "age" or "FU";trajectory will created by; FU : follow-up-time; x axis
#'@param trajectoryByUnit         character; "day","month","year"; x axis unit 
#'@param valueSelectMethod        character; "minimum" or "maximum"; value select method if multiple measurement values exist
#'@param ageAdjusted              logical; when you do lcmm, you want to adjust age?
#'@param genderAdjusted           logical; When you do lcmm, you want to adjust gender? 
#'@param degreeNum                vector; degree of formula
#'@param classNum                 vector; cluster number 
#'@param minimumMeasurementCount  numeric; minimum number of measurement count that each person must have
#'@param randomSeed               seed number
#'@export

createLCMMsetting <- function(targetCohortId,
                              measurementConceptId,
                              trajectoryDuration,
                              trajectoryBy = 'FU',
                              trajectoryByUnit = 'day',
                              valueSelectMethod = 'minimum',
                              ageAdjusted = FALSE,
                              genderAdjusted = FALSE,
                              degreeNum = 2:7,
                              classNum= 2:7,
                              minimumMeasurementCount,
                              randomSeed = NULL
                              ){
    
    temporalCovariateSetting <- FeatureExtraction::createTemporalCovariateSettings(useMeasurementValue = TRUE,
                                                                                   temporalStartDays = trajectoryDuration,
                                                                                   temporalEndDays = trajectoryDuration)
    if(trajectoryBy == 'FU'){
        demographicCovariateSetting <- FeatureExtraction::createCovariateSettings(useDemographicsAge = ageAdjusted,
                                                                                  useDemographicsGender = genderAdjusted)
    }else if(trajectoryBy == 'age'){
        demographicCovariateSetting <- FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE,
                                                                                  useDemographicsGender = genderAdjusted)
    }
    
    LCMMsettingList <- list(targetCohortId = targetCohortId,
                            measurementConceptId = measurementConceptId,
                            trajectoryDuration = trajectoryDuration,
                            trajectoryBy = trajectoryBy,
                            trajectoryByUnit = trajectoryByUnit,
                            valueSelectMethod = valueSelectMethod,
                            temporalSetting = temporalCovariateSetting,
                            demographicSetting = demographicCovariateSetting,
                            degreeNum = degreeNum,
                            classNum = classNum,
                            minimumMeasurementCount = minimumMeasurementCount,
                            randomSeed = randomSeed)
    
    return(LCMMsettingList)
} 

