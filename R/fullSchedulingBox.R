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
#'
#' @return
#' @export
#'
#' @examples
fullSchedulingBoxModuleServer <- function(id, input, output, session, schedulingDataBundle){
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

      #print(schedulingDataBundle$mcData)
      theRVData <- reactiveValues(mcData=schedulingDataBundle$mcData, afData=schedulingDataBundle$afData,
                                  combinedData=schedulingDataBundle$combinedData,
                                  ocData=schedulingDataBundle$ocData, leaveData=schedulingDataBundle$leaveData,
                                  viewSemester=202211, notes=schedulingDataBundle$notesData)



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
        )
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
        #cat(yellow("[App] modifiedData$mcData changed\n"))
        theRVData$mcData <- modifiedData$mcData
      })
      observeEvent(modifiedData1$mcData, {
        #cat(yellow("[App] modifiedData1$mcData changed\n"))
        #cat(red("here\n"))
        theRVData$mcData <- modifiedData1$mcData
      })
      observeEvent(modifiedData1$combinedData, {
        #cat(yellow("[App] modifiedData1$combinedData changed\n"))
        #cat(red("here\n"))
        theRVData$combinedData <- modifiedData1$combinedData
      })
      observeEvent(modifiedData2$combinedData, {
        #cat(yellow("[App] modifiedData2$combinedData changed\n"))
        theRVData$combinedData <- modifiedData2$combinedData
        if(!is.null(modifiedData2$afData)){
          theRVData$afData <- modifiedData2$afData
        }
        if(!is.null(modifiedData2$mcData)){
          theRVData$mcData <- modifiedData2$mcData
        }
        if(!is.null(modifiedData2$leaveData)){
          theRVData$leaveData <- modifiedData2$leaveData
        }
      })
      observeEvent(modifiedData$afData, {
        #cat(yellow("[App] modifiedData$afData changed\n"))
        theRVData$afData <- modifiedData$afData
      })
      observeEvent(modifiedData$combinedData, {
        #cat(yellow("[App] modifiedData$combinedData changed\n"))
        theRVData$combinedData <- modifiedData$combinedData
      })
      observeEvent(modifiedData3$mcData, {
        #cat(yellow("[App] modifiedData3$mcData changed\n"))
        theRVData$mcData <- modifiedData3$mcData
      })
      observeEvent(modifiedNotes$notes, {
        #cat(yellow("[App] modifiedNotes$notes changed: "))
        theRVData$notes <- modifiedNotes$notes
        #cat(green("allowUpdate = ", allowUpdate, "\n"))
        if(allowUpdate){
          saveRDS(theRVData$notes, ".//Data//schedulingNotes.rds")
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
                                                  allowUpdate=allowUpdate, rds.path=rds.path)
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
