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
                                  genderAdjusted = FALSE,
                                  minimumMeasurementCount = 2)

