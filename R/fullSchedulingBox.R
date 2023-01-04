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
      includeCSS(system.file("extdata", "DashboardModularized.css", package="Scheduling", mustWork=TRUE)),
      includeCSS(system.file("extdata", "scheduling.css", package = "Scheduling", mustWork=TRUE)),
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
                                          initialAllowUpdateDB){
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
      localAllowUpdateDB <- reactiveVal()

      allowUpdateDB <- FALSE

      # if (is.null(allowUpdateDB)){
      if(!allowUpdateDB){
        #############################################
        # This is the initial state that allows a   #
        # simple security test to allow updating    #
        # the remote database.                      #
        #############################################

        theLoginModal <- function() {
          ns <- session$ns
          modalDialog(
            tagList(
              div(
                h3("Enter password to allow editing data.  Otherwise click Cancel"),
                class="passwordBox"
              ),
              textInput(ns("thePassword"), "Password")
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitPassword"), "Submit")
            ),
            size="s"
          )
        }
        showModal(theLoginModal())
      }

      observeEvent(input$submitPassword, {
        cat("submitPassword clicked")
        # do the password check and if accurate then change the value of
        #localAllowUpdateDB(764)
        if(input$thePassword=="stove1"){
          allowUpdateDB <<- TRUE
          #cat("localAllowUPdateDB =", localAllowUpdateDB(), "\n")
          dbConn <<- dbConnect(RPostgres::Postgres(),
                               dbname = 'mcairns/geogscheduling', # database name
                               host = 'db.bit.io',
                               port = 5432,
                               user = 'mcairns',
                               password = "v2_3vDkz_xCxb4TxUfgZYdZ2Fa4X9pr6")
          removeModal()
        } else {
          cat(red("Incorrect Passord\n"))
        }
          cat("allowUpdateDB = ", allowUpdateDB, "\n")

      })
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
          current.semester1 <- paste(paste.year, t.letter, sep="")
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
          modifyRemoteDBTable(dbConn,
                              inData= modifiedData$mcData,
                              tableName="master_courses",
                              key="recnum")
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
          modifyRemoteDBTable(dbConn,
                              inData= modifiedData1$mcData,
                              tableName="master_courses",
                              key="recnum")
        }
      })
      observeEvent(modifiedData3$mcData, {
        #cat(yellow("[App] modifiedData3$mcData changed\n"))
        theRVData$mcData <- modifiedData3$mcData
        if(allowUpdate){
          saveRDS(theRVData$mcData, ".//Data//master.courses.rds")
        }
        if(allowUpdateDB){
          modifyRemoteDBTable(dbConn,
                              inData= modifiedData3$mcData,
                              tableName="master_courses",
                              key="recnum")
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
        if(!is.null(modifiedData$combinedData)){
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData$combinedData,
                                tableName="combined_data",
                                key="recnum")
          }
        }
      })
      observeEvent(modifiedData1$combinedData, {
        theRVData$combinedData <- modifiedData1$combinedData
        if(!is.null(modifiedData1$combinedData)){
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData1$combinedData,
                                tableName="combined_data",
                                key="recnum")
          }
        }
      })
      observeEvent(modifiedData2$combinedData, {
        cat(yellow("[App] modifiedData2$combinedData changed1\n"))
        cat(green("allowUpdateDB:", allowUpdateDB, "\n"))
        theRVData$combinedData <- modifiedData2$combinedData
        # storedNames <- names(theRVData$combinedData)
        # names(theRVData$combinedData) <- tolower(names(theRVData$combinedData))
        #browser()
        if(!is.null(modifiedData2$combinedData)){
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData2$combinedData,
                                tableName="combined_data",
                                key="recnum")
          }
        }

        if(!is.null(modifiedData2$mcData)){
          theRVData$mcData <- modifiedData2$mcData
          #browser()
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData2$mcData,
                                tableName="master_courses",
                                key="recnum")
          }
        }

        if(!is.null(modifiedData2$facultyUIN)){
          #theRVData$mcData <- modifiedData2$mcData
          #browser()
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData2$facultyUIN,
                                tableName="faculty_uin",
                                key="recnum")
          }
        }
        cat(yellow("[App] end of modifiedData2$combinedData oberverer.\n"))
      })

      observeEvent(modifiedData$afData, {
        theRVData$afData <- modifiedData$afData
        if(!is.null(modifiedData$afData)){
          if(allowUpdateDB){
            modifyRemoteDBTable(dbConn,
                                inData= modifiedData$afData,
                                tableName="available_faculty",
                                key="recnum")
          }
          theRVData$afData <- modifiedData$afData
        }
      })

      observeEvent(modifiedNotes$notes, {
        theRVData$notes <- modifiedNotes$notes
        if(allowUpdate){
          saveRDS(theRVData$notes, ".//Data//schedulingNotes.rds")
        }
        if(allowUpdateDB){
          modifyRemoteDBTable(dbConn,
                              inData= modifiedNotes$notes,
                              tableName="scheduling_notes",
                              key="recnum")
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
                                                  facultyUINs=reactive(theRVData$facultyUINs))
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
