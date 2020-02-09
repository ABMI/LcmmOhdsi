connectionDetails = DatabaseConnector::createConnectionDetails(dbms = 'sql server',
                                                               user = Sys.getenv("userID"),
                                                               password = Sys.getenv("userPW"),
                                                               server = Sys.getenv("server58"))
CDMschema = 'ICARUS.dbo'
Resultschema = 'ICARUS_Result.dbo'
cohortTable = 'CohortForTrajectory'

lcmm_setting <- createLCMMsetting(targetCohortId = 8000,
                                  measurementConceptId = 3011708,
                                  trajectoryDuration = 0:7300,
                                  trajectoryBy = 'age',
                                  trajectoryByUnit = 'year',
                                  valueSelectMethod = 'minimum',
                                  ageAdjusted = FALSE,
                                  genderAdjusted = TRUE,
                                  minimumMeasurementCount = 2)
longitudinalData <- getTrajectoryData(connectionDetails = connectionDetails,
                                      CDMschema = CDMschema,
                                      oracleTempSchema = NULL,
                                      Resultschema = Resultschema,
                                      cohortTable = cohortTable,
                                      temporalSetting = lcmm_setting$temporalSetting,
                                      demographicSetting = lcmm_setting$demographicSetting,
                                      TargetMeasurementConceptId = lcmm_setting$measurementConceptId,
                                      trajectoryBy = lcmm_setting$trajectoryBy,
                                      trajectoryByUnit = lcmm_setting$trajectoryByUnit,
                                      valueSelectMethod = lcmm_setting$valueSelectMethod,
                                      minimumMeasurementCount = lcmm_setting$minimumMeasurementCount,
                                      TargetCohortId = lcmm_setting$targetCohortId)

head(longitudinalData)
NumSet <- expand.grid(degreeNum = lcmm_setting$degreeNum, classNum = lcmm_setting$classNum) 
# NumSet[3,]$degreeNum
maxCores <- if(nrow(NumSet) < parallel::detectCores()-1){ nrow(NumSet) } else { parallel::detectCores()-1 }
cluster <- ParallelLogger::makeCluster(numberOfThreads = maxCores)
result <- ParallelLogger::clusterApply(cluster, 1:nrow(NumSet), 
                                       fun = function(x){doLCMM(NumSettingList = x,
                                                                data = longitudinalData)})
ParallerLogger::stopCluster(cluster)

