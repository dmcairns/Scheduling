#########################################
# Functions for interacting with a      #
# remote database.                      #
#########################################

#########################################
# Append, Delete, and Edit a table      #
# in a remote database.                 #
#########################################

#' Title
#'
#' @param dbConn database connection
#' @param inData revised data
#' @param tableName table in database for modification
#' @param key unique key
#'
#' @return
#' @export
#'
#' @examples
modifyRemoteDBTable <- function(dbConn, inData, tableName, key="recnum"){
  #determine difference between inData and the
  #remote data table.
  # if(tableName=="combined_data") {
  #   cat(green("browsing combined_data\n"))
  #   browser()
  # }

  #browser()
  query <- paste0("SELECT * FROM ", tableName)
  remoteData <- DBI::dbGetQuery(dbConn, sql(query))
  if(tableName == "combined_data") {
    inData <- inData %>%
      mutate(load=as.double(load))
    remoteData <- remoteData %>%
      mutate(load=as.double(load))
  }
  newData <- anti_join(inData, remoteData, by=key)
  newData <- unique(newData)

  if(nrow(newData)>0){
    #proceed with adding a record to the remote DB
      dbWriteTable(dbConn, tableName, newData, append=TRUE, row.names = FALSE)
      t.data <- DBI::dbGetQuery(dbConn, sql(query))
  } else {
    # check for record deletion
    newData <- anti_join(remoteData, inData, by=key)
    if(nrow(newData) > 0) {
      # proceed with deleting records from the remote DB
      for(i in 1:nrow(newData)){
        update_statement <- paste0("DELETE FROM ", tableName, " WHERE ", key, " = '", newData[i,key],"'")
        DBI::dbSendQuery(dbConn, update_statement)  #Should this be dbExecute
      }
    } else {
      # proceed with editing a record in the remote DB
        recordsWithChanges <- anti_join(inData, remoteData)
      if(nrow(recordsWithChanges)>0){
        for(i in 1:nrow(recordsWithChanges)){
          # update each element
          statementPrefix <- paste0("UPDATE ", tableName, " SET ")
          statementSuffix <- paste0("WHERE \"", key, "\" = '", recordsWithChanges[i,key], "'")
          theFields <- names(recordsWithChanges)
          theFields <- setdiff(theFields, key)
          update_statement <- statementPrefix
          for(j in 1:length(theFields)){
            #update_statement <- paste0(update_statement, "\"", theFields[j], "\" = '", recordsWithChanges[i, theFields[j]], "'")
            update_statement <- paste0(update_statement, "\"", theFields[j], "\"=")
            if(!is.na(recordsWithChanges[i, theFields[j]])){
              update_statement <- paste0(update_statement,"'", recordsWithChanges[i, theFields[j]], "'")
            } else {
              update_statement <- paste0(update_statement, "NULL")
            }
            if(j < length(theFields)){
              update_statement <- paste0(update_statement, ", ")
            }
          }
          # if(tableName=="combined_data") {
          #   cat(green("browsing combined_data\n"))
          #   browser()
          # }
          update_statement <- paste(update_statement, statementSuffix)
          DBI::dbExecute(dbConn, update_statement)
        }
      }
    }
  }

}


# dbConn <- dbConnect(RPostgres::Postgres(),
#                     dbname = 'mcairns/geogscheduling', # database name
#                     host = 'db.bit.io',
#                     port = 5432,
#                     user = 'mcairns',
#                     password = "v2_3vDkz_xCxb4TxUfgZYdZ2Fa4X9pr6")
#
# # query <- "SELECT * FROM faculty_leave"
# # facultyLeave <- DBI::dbGetQuery(dbConn, sql(query))
# facultyLeaveDeleteTest <- facultyLeave[c(1:nrow(facultyLeave)-1),]
# facultyLeaveEditTest <- facultyLeave %>%
#   mutate(semester=case_when(recnum=='39' ~ "Spring 2026",
#                             TRUE ~ semester))
# modifyRemoteDBTable(dbConn, inData=facultyLeaveEditTest, tableName="faculty_leave", key="recnum")
