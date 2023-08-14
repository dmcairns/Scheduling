################################
# Fix the load problem         #
################################
#
# ################################
# # Recover combinedData from DB #
# ################################
#
dbConn <- dbConnect(RMySQL::MySQL(),
                    dbname = 'DH_Admin_Data',
                    host = '128.194.19.22',
                    user = 'cairns',
                    password = "tWGYrRPDqtB9Eo3FJZ")

query <- "SELECT * FROM masterCourses"
masterCourses <- DBI::dbGetQuery(dbConn, sql(query))
query <- "SELECT * FROM facultyUIN"
theUINs <- DBI::dbGetQuery(dbConn, sql(query))
query <- "SELECT * FROM combinedData"
combinedData <- DBI::dbGetQuery(dbConn, sql(query))
# combinedDataModified <- combinedData %>%
#   select(-"row_names") %>%
#   rename(assigned.load=assignedLoad)
#
# query <- "SELECT * FROM testLoad"
# testLoad <- DBI:: dbGetQuery(dbConn, sql(query))
#
# ################################
# # create a new table on DB.    #
# ################################
# dbWriteTable(dbConn, "combinedData", combinedDataModified,
#              overwrite=TRUE, append = FALSE)
#
#update_statement <- "UPDATE combinedData SET `displayName`= \"Ma\", `UIN`=525004397 WHERE recnum=2321"
# update_statement <- "UPDATE masterCourses SET UIN=525004397 WHERE Faculty=\"Ma\""

#update_statement <- "UPDATE facultyUIN SET `UIN`=235002056 WHERE recnum=96"
#update_statement <- "UPDATE combinedData SET `displayName`= \"O'Reilly\", `Faculty`=\"O'Reilly, Kathleen\", `shortName`=\"O'Reilly\", `shortNameNoSpace`=\"O'Reilly\" WHERE UIN=617002996"
#update_statement <- "UPDATE masterCourses SET `instructor`= \"O'Reilly\",  `Faculty`=\"O'Reilly\" WHERE UIN=617002996"

update_statement <- "UPDATE combinedData SET `UIN`=235002056 WHERE displayName=\"Yadav\""

# # update_statement <- paste0("UPDATE testLoad SET `load`=5,", "`rank`=7 WHERE recnum=2322")
DBI::dbExecute(dbConn, update_statement)
# #
dbDisconnect(dbConn)
