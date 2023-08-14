
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjs)
library(readxl)
library(dplyr)
library(tidyr)
library(crayon)
library(DT)
library(lubridate)
library(stringr)
library(c3)
library(ggplot2)
library(htmltools)
library(shinyBS)
library(shinyWidgets)
library(tibble)
library(naniar)
library(Scheduling)
library(DBI)
library(dbplyr)
library(RMySQL)





ui <- dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title=tags$img(src="GeographyBannerSM.png", alt="Geography Banner"),
    leftUi = tagList(
      useShinyjs(),
      #remove CSS below for development
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "DashboardModularized.css?version=17")
      ),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "scheduling.css?version=6")
      )
    )
  ),
  sidebar = dashboardSidebar(


    minified = FALSE,
    collapsed = FALSE,   #Start with the sidebar collapsed
    id = "dashboardSideboardPageControls"
  ),
  body = dashboardBody(
    #tamuDashboardThemeGEOG,
    shinydashboardPlus::box(
      id = "readInputBox",
      title="Read Input",
      closable = TRUE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = FALSE,
      uiOutput("theReadUI"),
      sidebar = boxSidebar(
        id = "readInputSidebar",               #make sure that this is unique
        width = 25,                            #width can be no smaller than 25%
        p("readInput Sidebar Content")
      )
    ),
    fullSchedulingBoxModuleUI("testScheduling"),


    sidebar = boxSidebar(
      id = "theSidebar",                #make sure that this is unique
      width = 25,                              #width can be no smaller than 25%
      p("Sidebar Content")
    )
  )
)

server <- function(input, output, session) {
  drv <- RMySQL::MySQL()
  sapply(RMySQL::dbListConnections(drv), RMySQL::dbDisconnect) #Disconnects any open DB connections
  boxStatus <- reactiveValues(mybox=FALSE, programMetricsBox=FALSE,
                              researchMetricsBox=FALSE, gradStudentInfoBox=FALSE,
                              personnelBox=FALSE, gradProgramReportsBox=FALSE,
                              schedulingBox=FALSE, enrollmentBox=TRUE,
                              teachingBox=FALSE, evaluationBox=FALSE,
                              studentCommitteesBox=FALSE, readInputBox=FALSE,
                              overviewBox=FALSE, fullSCHBox=FALSE,
                              fullPersonnelBox=FALSE)
  classesStart <- data.frame(semester=c("202031", "202111", "202121", "202131", "202211", "202221", "202231", "202311", "202321"),
                             startDate=as.Date(c("2020-08-19", "2021-01-19", "2021-06-01", "2021-08-30", "2022-01-18", "2022-05-31", "2022-08-24", "2023-01-17", "2023-05-30")))

  # dbConn <- dbConnect(RPostgres::Postgres(),
  #                  dbname = 'mcairns/EnrollmentTracking', # database name
  #                  host = 'db.bit.io',
  #                  port = 5432,
  #                  user = 'mcairns',
  #                  password = "v2_3v5SD_cRWEpZn8CMgLfU4hNCbaXaH")

  dbConn <- dbConnect(RMySQL::MySQL(),
                      dbname = 'DH_Admin_Data',
                      host = '128.194.19.22',
                      user = 'cairns',
                      password = "tWGYrRPDqtB9Eo3FJZ")

  query <- "SELECT * FROM semestercodes"
  semesterCodes <- DBI::dbGetQuery(dbConn, sql(query))
  dbDisconnect(dbConn)

  # semesterCodes <- semesterCodes %>%
  #   mutate(Year=Year.1) %>%
  #   mutate(longSemester=longsemester)

  # dbConn <- dbConnect(RPostgres::Postgres(),
  #                  dbname = 'mcairns/geogscheduling', # database name
  #                  host = 'db.bit.io',
  #                  port = 5432,
  #                  user = 'mcairns',
  #                  password = "v2_3vDkz_xCxb4TxUfgZYdZ2Fa4X9pr6")

  dbConn <- dbConnect(RMySQL::MySQL(),
                      dbname = 'DH_Admin_Data',
                      host = '128.194.19.22',
                      user = 'cairns',
                      password = "tWGYrRPDqtB9Eo3FJZ")

  #query <- "SELECT * FROM available_faculty"
  query <- "SELECT * FROM availableFaculty"
  availableFaculty <- DBI::dbGetQuery(dbConn, sql(query))

  availableFaculty <- availableFaculty %>%
    #rename(Faculty=faculty) %>%
    select(!contains("Unnamed"))

  # query <- "SELECT * FROM combined_data"
  query <- "SELECT * FROM combinedData"
  combinedData <- DBI::dbGetQuery(dbConn, sql(query))

  combinedData <- combinedData %>%
    # rename(longSemester=longsemester) %>%
    # rename(shortName=shortname) %>%
    # rename(shortSemester=shortsemester) %>%
    # rename(Faculty=faculty) %>%
    # rename(numericSemester=numericsemester) %>%
    # rename(shortNameNoSpace=shortnamenospace) %>%
    select(!contains("Unnamed"))

  # query <- "SELECT * FROM faculty_leave"
  query <- "SELECT * FROM facultyLeave"
  facultyLeave <- DBI::dbGetQuery(dbConn, sql(query))
  facultyLeave <- facultyLeave %>%
    #rename(leaveType=leavetype) %>%
    select(!contains("Unnamed"))

  # query <- "SELECT * FROM master_courses"
  query <- "SELECT * FROM masterCourses"
  masterCourses <- DBI::dbGetQuery(dbConn, sql(query))

  masterCourses <- masterCourses %>%
    # rename(courseID=courseid) %>%
    # rename(Description=description) %>%
    # rename(OL=ol) %>%
    # rename(W=w) %>%
    # rename(C=c) %>%
    # rename(H=h) %>%
    # rename(SA=sa) %>%
    # rename(Faculty=faculty)%>%
    select(!contains("Unnamed"))

  # query <- "SELECT * FROM scheduling_notes"
  query <- "SELECT * FROM schedulingNotes"
  schedulingNotes <- DBI::dbGetQuery(dbConn, sql(query))
  schedulingNotes <- schedulingNotes %>%
    select(!contains("Unnamed"))

  # query <- "SELECT * FROM faculty_uin"
  query <- "SELECT * FROM facultyUIN"
  facultyUIN <- DBI::dbGetQuery(dbConn, sql(query))


  schedulingDataBundle <- list(mcData=masterCourses,
                               afData=availableFaculty,
                               combinedData=combinedData,
                               semester.codes=semesterCodes,
                               leaveData=facultyLeave,
                               notesData=schedulingNotes,
                               course.inventory=NULL,
                               facultyUINs=facultyUIN)


  fullSchedulingBoxModuleServer("testScheduling", schedulingDataBundle=schedulingDataBundle,
                                boxArrangement="new", initialAllowUpdateDB=TRUE)



}


# Run the application
shinyApp(ui = ui, server = server)
