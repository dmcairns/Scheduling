#' fullSchedulingBoxModuleUI
#'
#' @param id The element id
#'
#' @return
#' @export
#'
#' @examples
fullSchedulingBoxModuleUI <- function(id){
  ns <- shiny::NS(id)
  shinydashboardPlus::box(
    id = "schedulingBox",
    title = "Scheduling Tools",
    closable = TRUE,
    width = 12,
    height = "500px",
    solidHeader = FALSE,
    collapsible = FALSE,
    tagList(
      shiny::uiOutput(ns("schedulingTools"))
    ),
    sidebar = boxSidebar(
      id = "schedulingSidebar",                #make sure that this is unique
      width = 25,                              #width can be no smaller than 25%
      tagList(
        shiny::uiOutput(ns("sidebarSemesterUI")),
        shiny::uiOutput(ns("synchronize"))

      )
    )
  )

}

#' fullSchedulingBoxModuleServer
#'
#' @param id The element id
#' @param input The input element
#' @param output The output element
#' @param session The session
#' @param schedulingDataBundle The entire dataset
#' @param boxArrangement the arrangement
#' @param allowUpdateDB allow update of remote database.
#'
#' @return
#' @export
#'
#' @examples
fullSchedulingBoxModuleServer <- function(id, input, output, session, schedulingDataBundle, boxArrangement="classic",
                                          allowUpdateDB){
  moduleServer(
    id,
    function(input, output, session){
      #cat(yellow("[fullSchedulingBoxModuleServer]\n"))
      allowUpdate <- FALSE   #Doesn't work if set to TRUE
      #cat(yellow("[fullSchedulingBoxModuleServer]"), green("allowUpdate:", allowUpdate, "\n"))
      ns <- session$ns
      rvFocalSemester <- reactiveVal(NA)
      rvReferenceSemester <- reactiveVal(NA)
      rvDepartment <- reactiveVal(NA)
      rvCourse <- reactiveVal(NA)
      rvSingleFaculty <- reactiveVal(NA)
      testRV <- reactiveValues()
      testRV$dt <- data.frame(a=c(1,2), b=c(3,4))
      rv <- reactiveValues()
      rvOutput <- reactiveValues()
      read.legacy <- FALSE
      accessRemoteData <- FALSE
      rds.path <- ".//Data//"
      semester.codes <- schedulingDataBundle$semester.codes

      if (allowUpdateDB){
        dbConn <- dbConnect(RPostgres::Postgres(),
                            dbname = 'mcairns/geogscheduling', # database name
                            host = 'db.bit.io',
                            port = 5432,
                            user = 'mcairns',
                            password = "v2_3vDkz_xCxb4TxUfgZYdZ2Fa4X9pr6")
      }

      theRVData <- reactiveValues(mcData=schedulingDataBundle$mcData, afData=schedulingDataBundle$afData,
                                  combinedData=schedulingDataBundle$combinedData,
                                  ocData=schedulingDataBundle$ocData, leaveData=schedulingDataBundle$leaveData,
                                  viewSemester=202211, notes=schedulingDataBundle$notesData,
                                  facultyUINs=schedulingDataBundle$facultyUINs)



      create.course.inventory <- function(courseData){
        # Create a Course Inventory based on the master data
        #
        courses <- unique(courseData$courseID)
        courses <- sort(courses)
        courses <- data.frame(course=courses)

        courseData <- courseData %>%
          select(courseID, Description)
        t.duplicated <- duplicated(courseData)
        courseData <- courseData[!t.duplicated,]
        courseData <- courseData[order(courseData$courseID),]
        courseData
      }
      CurrentSemester <- function(numeric=FALSE, digits=4){
        t.year <- year(Sys.Date())
        year.digit <- t.year

        two.digit.year <- year.digit %% 1000
        t.c <- paste(t.year,"-12-15", sep="")
        t.b <- paste(t.year,"-8-25", sep="")
        t.a <- paste(t.year,"-5-15", sep="")
        current.date <- as.character(Sys.Date())
        if(digits==4) {paste.year <- year.digit}
        if(digits==2) {paste.year <- two.digit.year}
        if(numeric){
          paste.year <- year.digit
          if(current.date < t.a) {
            t.letter <- "11"
          } else if (current.date < t.b) {
            t.letter <- "21"
          } else t.letter <- "31"
          current.semester1 <- paste(paste.year, t.letter, sep="")
          current.semester1 <- as.numeric(current.semester1)
        } else{
          if(current.date < t.a) {
            t.letter <- "A"
          } else if (current.date < t.b) {
            t.letter <- "B"
          } else t.letter <- "C"
          current.semester1 <- paste(paste.year+1, t.letter, sep="")
        }
        current.semester1
      }
      output$schedulingTools <- renderUI({
        if(boxArrangement=="classic"){
          #cat(yellow("[schedulingTools1]\n"))
          tagList(
            fluidRow(
              column(6,
                     fluidRow(courseListingUI(ns("courseList"))),
                     fluidRow(instructorAssignmentModuleUI(ns("assignment"))),
                     fluidRow(instructorLoadModuleUI(ns("load")))),
              column(6,
                     fluidRow(
                       column(12, reportModuleUI(ns("testReport"))),
                       column(12, reportUnassignedCoursesModuleUI(ns("unassigned"))),
                       column(12, reportFacultyModuleUI(ns("singleFaculty"))),
                       column(12, reportMultiSectionModuleUI(ns("multiSection"))),
                       column(12, reportNotesModuleUI(ns("notes"))),
                       #column(12, reportLongModuleUI("long")),
                       column(12, reportGraphicalScheduleModuleUI(ns("graphicalSchedule")))
                     )
              )
            )
          )} else {
            tagList(
              fluidRow(
                column(12,
                       reportModuleUI(ns("testReport")))
              ),
              fluidRow(
                column(6,
                       fluidRow(courseListingUI(ns("courseList"))),
                       fluidRow(instructorAssignmentModuleUI(ns("assignment"))),
                       fluidRow(instructorLoadModuleUI(ns("load")))),
                column(6,
                       fluidRow(
                         column(12, reportUnassignedCoursesModuleUI(ns("unassigned"))),
                         column(12, reportFacultyModuleUI(ns("singleFaculty"))),
                         column(12, reportMultiSectionModuleUI(ns("multiSection"))),
                         column(12, reportNotesModuleUI(ns("notes"))),
                         #column(12, reportLongModuleUI("long")),
                         column(12, reportGraphicalScheduleModuleUI(ns("graphicalSchedule")))
                       )
                )
              )
            )
          }
      })

      output$sidebarSemesterUI <- renderUI({
        ns <- session$ns
        #cat(yellow("[fullSchedulingBoxModuleServer]"), green("output$sidebarSemesterUI\n"))
        #print(schedulingDataBundle$semester.codes)
        #print(names(schedulingDataBundle))
        #assign("t.1", schedulingDataBundle, pos=1)
        #cat(yellow("[fullSchedulingBoxModuleServer]"), green("after print semester.codes\n"))

        currentYear <- year(today())
        currentYearPlus5 <- currentYear+5
        useSemesters <- schedulingDataBundle$semester.codes %>%
          filter(Year <= currentYearPlus5) %>%
          filter(as.character(current) >= "201431")


        chosen.semester <- useSemesters %>%
          filter(semester4==CurrentSemester()) %>%
          select("semester.display") %>%
          unlist() %>%
          as.vector()

        #cat(blue("chosen.semester:", chosen.semester, "\n"))
        t.out <- div(id=ns("sidebarSemesterDiv"), class="semesterSelect",
                     selectInput(ns("sidebarSemester"), "Semester",
                                 useSemesters$semester.display, selected = chosen.semester)
        )
        t.out
      })
      output$synchronize <- renderUI({
        ns <- session$ns
        materialSwitch(ns("synchSwitch"), value=TRUE, label="Synchronize")
      })

      schedulingDataBundle$course.inventory <- create.course.inventory(schedulingDataBundle$mcData)

      fixNamesNoSpace <- function(inData){
        outData <- gsub(". ", "", inData, fixed=TRUE)
        outData <- gsub("'", "", outData, fixed=TRUE)  #New addition to solve problem with adding employment
        #outData <- gsub("’", "", outData, fixed=TRUE)  #New addition to solve problem with adding employment
        outData <- gsub(" ", "", outData, fixed=TRUE)
        outData
      }

      modifiedData <- courseListingServer("courseList", inSemesterCodes=semester.codes,
                                          theMasterCourses=reactive(theRVData$mcData),
                                          theFaculty=reactive(theRVData$afData),
                                          theCoursesOffered=reactive(theRVData$ocData),
                                          theCombinedData=reactive(theRVData$combinedData),
                                          theCourseInventory=schedulingDataBundle$course.inventory,
                                          showDebugCues=showDebugCues,
                                          synchronize=reactive(input$synchSwitch),
                                          chosenSemester=reactive(input$sidebarSemester))


      observeEvent(modifiedData$mcData, {
        cat(yellow("[App] modifiedData$mcData changed\n"))
        theRVData$mcData <- modifiedData$mcData
        if(allowUpdate){
          saveRDS(theRVData$mcData, ".//Data//master.courses.rds")
        }
        if(allowUpdateDB){
          #determine difference between existing master_courses data
          #and that stored on the DB.
          #browser()
          query <- "SELECT * FROM master_courses"
          masterCourses <- DBI::dbGetQuery(dbConn, sql(query))
          newData <- anti_join(theRVData$mcData, masterCourses, by=names(masterCourses)[1])
          newData <- unique(newData)
          names(newData) <- tolower(names(newData))
          #browser()
          if(nrow(newData)>0){
            #proceed with adding a record to the remote DB

            dbWriteTable(dbConn, "master_courses", newData, append=TRUE, row.names = FALSE)
            t.data <- DBI::dbGetQuery(dbConn, sql(query))
          } else {
            newData <- anti_join(masterCourses, theRVData$mcData, by=names(masterCourses)[1])
            if(nrow(newData) > 0) {

              # proceed with deleting a record from the remote DB
              update_statement <- paste0("DELETE FROM master_courses WHERE recnum = '", newData$recnum,"'")
              dbSendQuery(dbConn, update_statement)
            } else {
              # proceed with editing a record in the remote DB
              recordsWithChanges <- anti_join(theRVData$mcData, masterCourses)
              for(i in 1:nrow(recordsWithChanges)){
                # update each element
                update_statement <- paste0("UPDATE master_courses SET department = '", recordsWithChanges$Department[i], "', ",
                                           "course = '", recordsWithChanges$Course[i], "', ",
                                           "description = '", recordsWithChanges$Description[i], "', ",
                                           "semester = '", recordsWithChanges$semester[i], "'. ",
                                           "instructor = '", recordsWithChanges$instructor[i], "', ",
                                           "courseid = '", recordsWithChanges$courseid[i], "', ",
                                           "section = ", recordsWithChanges$section[i], ", ",
                                           "w = ", recordsWithChanges$w[i], ", ",
                                           "c = ", recordsWithChanges$c[i], ", ",
                                           "ol = ", recordsWithChanges$ol[i], ", ",
                                           "h = ", recordsWithChanges$h[i], ", ",
                                           "sa = ", recordsWithChanges$sa[i], ", ",
                                           "stacked = '", recordsWithChanges$stacked[i], "', ",
                                           "load.contribution = ", recordsWithChanges$load.contribution[i], ", ",
                                           "faculty = '", recordsWithChanges$faculty[i], "' ",
                                           "WHERE recnum = '", recordsWithChanges$recnum[i], "'")
                dbSendQuery(dbConn, update_statement)
              }
            }
          }

        }
      })
      observeEvent(modifiedData1$mcData, {
        #cat(yellow("[App] modifiedData1$mcData changed\n"))
        #cat(red("here\n"))
        theRVData$mcData <- modifiedData1$mcData
        if(allowUpdate){
          saveRDS(theRVData$mcData, ".//Data//master.courses.rds")
        }
        if(allowUpdateDB){
          #determine difference between existing master_courses data
          #and that stored on the DB.
          #browser()
          query <- "SELECT * FROM master_courses"
          masterCourses <- DBI::dbGetQuery(dbConn, sql(query))
          newData <- anti_join(theRVData$mcData, masterCourses, by=names(masterCourses)[1])
          newData <- unique(newData)
          names(newData) <- tolower(names(newData))
          #browser()
          if(nrow(newData)>0){
            #proceed with adding a record to the remote DB

            dbWriteTable(dbConn, "master_courses", newData, append=TRUE, row.names = FALSE)
            t.data <- DBI::dbGetQuery(dbConn, sql(query))
          } else {
            newData <- anti_join(masterCourses, theRVData$mcData, by=names(masterCourses)[1])
            if(nrow(newData) > 0) {

              # proceed with deleting a record from the remote DB
              update_statement <- paste0("DELETE FROM master_courses WHERE recnum = '", newData$recnum,"'")
              dbSendQuery(dbConn, update_statement)
            } else {
              # proceed with editing a record in the remote DB
              recordsWithChanges <- anti_join(theRVData$mcData, masterCourses)
              for(i in 1:nrow(recordsWithChanges)){
                # update each element
                update_statement <- paste0("UPDATE master_courses SET department = '", recordsWithChanges$Department[i], "', ",
                                           "course = '", recordsWithChanges$Course[i], "', ",
                                           "description = '", recordsWithChanges$Description[i], "', ",
                                           "semester = '", recordsWithChanges$semester[i], "'. ",
                                           "instructor = '", recordsWithChanges$instructor[i], "', ",
                                           "courseid = '", recordsWithChanges$courseid[i], "', ",
                                           "section = ", recordsWithChanges$section[i], ", ",
                                           "w = ", recordsWithChanges$w[i], ", ",
                                           "c = ", recordsWithChanges$c[i], ", ",
                                           "ol = ", recordsWithChanges$ol[i], ", ",
                                           "h = ", recordsWithChanges$h[i], ", ",
                                           "sa = ", recordsWithChanges$sa[i], ", ",
                                           "stacked = '", recordsWithChanges$stacked[i], "', ",
                                           "load.contribution = ", recordsWithChanges$load.contribution[i], ", ",
                                           "faculty = '", recordsWithChanges$faculty[i], "' ",
                                           "WHERE recnum = '", recordsWithChanges$recnum[i], "'")
                dbSendQuery(dbConn, update_statement)
              }
            }
          }

        }
      })
      observeEvent(modifiedData3$mcData, {
        #cat(yellow("[App] modifiedData3$mcData changed\n"))
        theRVData$mcData <- modifiedData3$mcData
        if(allowUpdate){
          saveRDS(theRVData$mcData, ".//Data//master.courses.rds")
        }
        if(allowUpdateDB){
          #determine difference between existing master_courses data
          #and that stored on the DB.
          #browser()
          query <- "SELECT * FROM master_courses"
          masterCourses <- DBI::dbGetQuery(dbConn, sql(query))
          newData <- anti_join(theRVData$mcData, masterCourses, by=names(masterCourses)[1])
          newData <- unique(newData)
          names(newData) <- tolower(names(newData))
          #browser()
          if(nrow(newData)>0){
            #proceed with adding a record to the remote DB

            dbWriteTable(dbConn, "master_courses", newData, append=TRUE, row.names = FALSE)
            t.data <- DBI::dbGetQuery(dbConn, sql(query))
          } else {
            newData <- anti_join(masterCourses, theRVData$mcData, by=names(masterCourses)[1])
            if(nrow(newData) > 0) {

              # proceed with deleting a record from the remote DB
              update_statement <- paste0("DELETE FROM master_courses WHERE recnum = '", newData$recnum,"'")
              dbSendQuery(dbConn, update_statement)
            } else {
              # proceed with editing a record in the remote DB
              recordsWithChanges <- anti_join(theRVData$mcData, masterCourses)
              for(i in 1:nrow(recordsWithChanges)){
                # update each element
                update_statement <- paste0("UPDATE master_courses SET department = '", recordsWithChanges$Department[i], "', ",
                                           "course = '", recordsWithChanges$Course[i], "', ",
                                           "description = '", recordsWithChanges$Description[i], "', ",
                                           "semester = '", recordsWithChanges$semester[i], "'. ",
                                           "instructor = '", recordsWithChanges$instructor[i], "', ",
                                           "courseid = '", recordsWithChanges$courseid[i], "', ",
                                           "section = ", recordsWithChanges$section[i], ", ",
                                           "w = ", recordsWithChanges$w[i], ", ",
                                           "c = ", recordsWithChanges$c[i], ", ",
                                           "ol = ", recordsWithChanges$ol[i], ", ",
                                           "h = ", recordsWithChanges$h[i], ", ",
                                           "sa = ", recordsWithChanges$sa[i], ", ",
                                           "stacked = '", recordsWithChanges$stacked[i], "', ",
                                           "load.contribution = ", recordsWithChanges$load.contribution[i], ", ",
                                           "faculty = '", recordsWithChanges$faculty[i], "' ",
                                           "WHERE recnum = '", recordsWithChanges$recnum[i], "'")
                dbSendQuery(dbConn, update_statement)
              }
            }
          }

        }
      })

      observeEvent(modifiedData2$leaveData, {
        cat(green("The leave data has been changed1.\n"))

        if(!is.null(modifiedData2$leaveData)){
          cat(red("leaveData is modified1\n"))
          if(allowUpdate){
            saveRDS(modifiedData2$leaveData, ".//Data//facultyLeave.rds")
          }
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData2$leaveData,
                                tableName="faculty_leave",
                                key="recnum")
          }
        }
        theRVData$leaveData <- modifiedData2$leaveData
      })
      observeEvent(modifiedData$combinedData, {
        #cat(yellow("[App] modifiedData$combinedData changed\n"))
        theRVData$combinedData <- modifiedData$combinedData
      })
      observeEvent(modifiedData1$combinedData, {
        #cat(yellow("[App] modifiedData1$combinedData changed\n"))
        #cat(red("here\n"))
        theRVData$combinedData <- modifiedData1$combinedData
      })
      observeEvent(modifiedData2$combinedData, {
        cat(yellow("[App] modifiedData2$combinedData changed1\n"))
        #browser()
        theRVData$combinedData <- modifiedData2$combinedData
        storedNames <- names(theRVData$combinedData)
        names(theRVData$combinedData) <- tolower(names(theRVData$combinedData))
        #localCombinedData <- modifiedData2$combinedData
        #names(localCombinedData) <- tolower(names(localCombinedData))
        #browser()
        if(!is.null(modifiedData2$combinedData)){
          if(allowUpdateDB){
            #determine difference between existing combinedData data
            #and that stored on the DB.

            query <- "SELECT * FROM combined_data"
            combinedData <- DBI::dbGetQuery(dbConn, sql(query))
            newData <- anti_join(theRVData$combinedData, combinedData, by=names(combinedData)[1])
            #newData <- anti_join(localCombinedData, combinedData, by=names(combinedData)[1])
            newData <- unique(newData)
            names(newData) <- tolower(names(newData))
            combinedData <- combinedData %>%
              mutate(numericsemester=as.integer(numericsemester)) %>%
              mutate(load=as.numeric(load))
            if(nrow(newData)>0){
              #proceed with adding a record to the remote DB

              dbWriteTable(dbConn, "combined_data", newData, append=TRUE, row.names = FALSE)
              t.data <- DBI::dbGetQuery(dbConn, sql(query))
            } else {
              newData <- anti_join(combinedData, theRVData$combinedData, by=names(combinedData)[1])
              #newData <- anti_join(combinedData, localCombinedData, by=names(combinedData)[1])
              if(nrow(newData) > 0) {

                # proceed with deleting a record from the remote DB
                update_statement <- paste0("DELETE FROM master_courses WHERE recnum = '", newData$recnum,"'")
                dbSendQuery(dbConn, update_statement)
              } else {
                # proceed with editing a record in the remote DB 1
                recordsWithChanges <- anti_join(theRVData$combinedData, combinedData)
                #recordsWithChanges <- anti_join(localCombinedData, combinedData)
                cat("past the join\n")
                if(nrow(recordsWithChanges)>0){
                  #browser()
                  for(i in 1:nrow(recordsWithChanges)){
                    # update each element
                    # below works, but there is a problem with updating assigned.load.  I think it's because of the period in the name.
                    update_statement <- paste0("UPDATE combined_data SET faculty = '", recordsWithChanges$faculty[i], "', ",
                                               "shortname = '", recordsWithChanges$shortname[i], "', ",
                                               "shortnamenospace = '", recordsWithChanges$shortnamenospace[i], "', ",
                                               "longsemester = '", recordsWithChanges$longsemester[i], "', ",
                                               "shortsemester = '", recordsWithChanges$shortsemester[i], "', ",
                                               "numericsemester = '", recordsWithChanges$numericsemester[i], "', ",
                                               "sem2 = '", recordsWithChanges$sem2[i], "', ",
                                               "rank = ", recordsWithChanges$rank[i], ", ",
                                               "load = ", recordsWithChanges$load[i], " ",
                                               "WHERE recnum = '", recordsWithChanges$recnum[i], "'")
                    dbSendQuery(dbConn, update_statement)
                  }
                }
              }
            }

          }
          names(theRVData$combinedData) <- storedNames
        }
        if(!is.null(modifiedData2$afData)){
          theRVData$afData <- modifiedData2$afData
        }
        if(!is.null(modifiedData2$mcData)){
          theRVData$mcData <- modifiedData2$mcData
          if(allowUpdateDB){
            #determine difference between existing master_courses data
            #and that stored on the DB.
            #browser()
            query <- "SELECT * FROM master_courses"
            masterCourses <- DBI::dbGetQuery(dbConn, sql(query))
            newData <- anti_join(theRVData$mcData, masterCourses, by=names(masterCourses)[1])
            newData <- unique(newData)
            names(newData) <- tolower(names(newData))
            #browser()
            if(nrow(newData)>0){
              #proceed with adding a record to the remote DB

              dbWriteTable(dbConn, "master_courses", newData, append=TRUE, row.names = FALSE)
              t.data <- DBI::dbGetQuery(dbConn, sql(query))
            } else {
              newData <- anti_join(masterCourses, theRVData$mcData, by=names(masterCourses)[1])
              if(nrow(newData) > 0) {

                # proceed with deleting a record from the remote DB
                update_statement <- paste0("DELETE FROM master_courses WHERE recnum = '", newData$recnum,"'")
                dbSendQuery(dbConn, update_statement)
              } else {
                # proceed with editing a record in the remote DB 1
                recordsWithChanges <- anti_join(theRVData$mcData, masterCourses)
                if(nrow(recordsWithChanges)>0){
                  for(i in 1:nrow(recordsWithChanges)){
                    # update each element
                    update_statement <- paste0("UPDATE master_courses SET department = '", recordsWithChanges$Department[i], "', ",
                                               "course = '", recordsWithChanges$Course[i], "', ",
                                               "description = '", recordsWithChanges$Description[i], "', ",
                                               "semester = '", recordsWithChanges$semester[i], "'. ",
                                               "instructor = '", recordsWithChanges$instructor[i], "', ",
                                               "courseid = '", recordsWithChanges$courseid[i], "', ",
                                               "section = ", recordsWithChanges$section[i], ", ",
                                               "w = ", recordsWithChanges$w[i], ", ",
                                               "c = ", recordsWithChanges$c[i], ", ",
                                               "ol = ", recordsWithChanges$ol[i], ", ",
                                               "h = ", recordsWithChanges$h[i], ", ",
                                               "sa = ", recordsWithChanges$sa[i], ", ",
                                               "stacked = '", recordsWithChanges$stacked[i], "', ",
                                               "load.contribution = ", recordsWithChanges$load.contribution[i], ", ",
                                               "faculty = '", recordsWithChanges$faculty[i], "' ",
                                               "WHERE recnum = '", recordsWithChanges$recnum[i], "'")
                    dbSendQuery(dbConn, update_statement)
                  }
                }
              }
            }

          }
        }
      })

      observeEvent(modifiedData$afData, {
        #cat(yellow("[App] modifiedData$afData changed\n"))
        theRVData$afData <- modifiedData$afData
      })

      observeEvent(modifiedNotes$notes, {
        theRVData$notes <- modifiedNotes$notes
        if(allowUpdate){
          saveRDS(theRVData$notes, ".//Data//schedulingNotes.rds")
        }
        if(allowUpdateDB){
          #determine difference between existing schedulingNotes data
          #and that stored on the DB.
          query <- "SELECT * FROM scheduling_notes"
          schedulingNotes <- DBI::dbGetQuery(dbConn, sql(query))
          newData <- anti_join(theRVData$notes, schedulingNotes, by=names(schedulingNotes)[1])
          if(nrow(newData)>0){
            #proceed with adding a record to the remote DB
            dbWriteTable(dbConn, "scheduling_notes", newData, append=TRUE, row.names = FALSE)
            t.data <- DBI::dbGetQuery(dbConn, sql(query))
          } else {
              newData <- anti_join(schedulingNotes, theRVData$notes, by=names(schedulingNotes)[1])
              if(nrow(newData) > 0) {
                # proceed with deleting a record from the remote DB
                update_statement <- paste0("DELETE FROM scheduling_notes WHERE recnum = ", newData$recnum)
                dbSendQuery(dbConn, update_statement)
              } else {
                # proceed with editing a record in the remote DB
                recordsWithChanges <- anti_join(theRVData$notes, schedulingNotes)
                for(i in 1:nrow(recordsWithChanges)){
                  # update each element
                  update_statement <- paste0("UPDATE scheduling_notes SET semester = '", recordsWithChanges$semester[i], "', ",
                                             "note = '", recordsWithChanges$note[i], "', ",
                                             "source = '", recordsWithChanges$source[i], "' ",
                                             "WHERE recnum = ", recordsWithChanges$recnum[i])
                  dbSendQuery(dbConn, update_statement)
                }
              }
          }

        }

      })

      #cat(yellow("[fullSchedulingBoxModuleServer]"), green("Before modules\n"))
      reportModuleServer("testReport", inSemester=reactive(input$sidebarSemester),
                         theMasterCourses=reactive(theRVData$mcData),
                         theCombinedData=reactive(theRVData$combinedData),
                         theLeaveData=reactive(theRVData$leaveData),
                         inSemesterCodes=semester.codes,
                         synchronize=reactive(input$synchSwitch),
                         chosenSemester=reactive(input$sidebarSemester)
      )
      #cat(yellow("[fullSchedulingBoxModuleServer]"), green("After reportModuleServer\n"))
      reportUnassignedCoursesModuleServer("unassigned", inSemester=reactive(input$sidebarSemester),
                                          theMasterCourses=reactive(theRVData$mcData),
                                          theCombinedData=reactive(theRVData$combinedData),
                                          theLeaveData=reactive(theRVData$leaveData),
                                          inSemesterCodes=semester.codes,
                                          synchronize=reactive(input$synchSwitch),
                                          chosenSemester=reactive(input$sidebarSemester),
                                          suspendWhenHidden=reactive(input$suspendWhenHidden)
      )
      #cat(yellow("[fullSchedulingBoxModuleServer]"), green("After reportUnassignedCoursesModuleServer\n"))
      rvSingleFaculty <- reportFacultyModuleServer("singleFaculty", inSemester=reactive(input$sidebarSemester),
                                                   theMasterCourses=reactive(theRVData$mcData),
                                                   theCombinedData=reactive(theRVData$combinedData),
                                                   theLeaveData=reactive(theRVData$leaveData),
                                                   inSemesterCodes=semester.codes,
                                                   inSelectedFaculty = reactive(rvSingleFaculty),
                                                   allowUpdate=allowUpdate, rds.path=rds.path)
      # #cat(yellow("[fullSchedulingBoxModuleServer]"), green("After reportFacultyModuleServer\n"))
      reportMultiSectionModuleServer("multiSection", inSemester=reactive(input$sidebarSemester),
                                     theMasterCourses=reactive(theRVData$mcData),
                                     theCombinedData=reactive(theRVData$combinedData),
                                     theLeaveData=reactive(theRVData$leaveData),
                                     inSemesterCodes=semester.codes)
      # #cat(yellow("[fullSchedulingBoxModuleServer]"), green("After reportMultiSectionModuleServer\n"))
      modifiedData3 <- reportGraphicalScheduleModuleServer("graphicalSchedule", inSemester=reactive(input$sidebarSemester),
                                                           theMasterCourses=reactive(theRVData$mcData),
                                                           theCombinedData=reactive(theRVData$combinedData),
                                                           theLeaveData=reactive(theRVData$leaveData),
                                                           theCourseInventory=schedulingDataBundle$course.inventory,
                                                           inSemesterCodes=semester.codes)
      # #cat(yellow("[fullSchedulingBoxModuleServer]"), green("After reportGraphicalScheduleModuleServer\n"))
      modifiedData1 <- instructorAssignmentModuleServer("assignment", inSemester=reactive(input$sidebarSemester),
                                                        theMasterCourses=reactive(theRVData$mcData),
                                                        theCombinedData=reactive(theRVData$combinedData),
                                                        theLeaveData=reactive(theRVData$leaveData),
                                                        inSemesterCodes=semester.codes,
                                                        synchronize=reactive(input$synchSwitch),
                                                        chosenSemester=reactive(input$sidebarSemester),
                                                        suspendWhenHidden=reactive(input$suspendWhenHidden),
                                                        allowUpdate=allowUpdate, rds.path=rds.path)
      modifiedData2 <- instructorLoadModuleServer("load", inSemester=reactive(input$sidebarSemester),
                                                  theMasterCourses=reactive(theRVData$mcData),
                                                  theCombinedData=reactive(theRVData$combinedData),
                                                  theLeaveData=reactive(theRVData$leaveData),
                                                  theAvailableFacultyData=reactive(theRVData$afData),
                                                  inSemesterCodes=semester.codes,
                                                  allowUpdate=allowUpdate, rds.path=rds.path,
                                                  facultyUINs=theRVData$facultyUINs)
      #cat("Before reportNotesModuleServer\n")
      modifiedNotes <- reportNotesModuleServer("notes", inSemester=reactive(input$sidebarSemester),
                                               theNotes = reactive(theRVData$notes),
                                               theCombinedData = reactive(theRVData$combinedData),
                                               allowUpdate=allowUpdate, rds.path=rds.path)
      #cat("After reportNotesModuleServer\n")



      outputOptions(output, "sidebarSemesterUI", suspendWhenHidden = FALSE)
    }
  )
}
