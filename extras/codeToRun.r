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
