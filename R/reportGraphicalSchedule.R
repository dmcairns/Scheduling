#' reportGraphicalScheduleModuleUI
#'
#' @param id the id
#'
#' @return
#' @export
#'
#' @examples
reportGraphicalScheduleModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("graphicalScheduleSidebar")),
                     width=40,
                     background="#998542",
                     id=ns("legacyReportSidebarControls"),
                     class="testSidebar"
                   ),
                   tagList(
                     htmltools::div(class="aBoxBodyC3Background",
                                    fluidRow(
                                      column(12, div(id=ns("graphicalSchedule"), class="graphicalScheduleTable",
                                                     dataTableOutput(ns("graphicalSchedule"))))
                                    ),
                                    tags$script(paste0("$(document).on('click', '.graphicalScheduleTable button', function(){
                                           Shiny.onInputChange('", ns("lastClickId"), "',this.id);
                                           Shiny.onInputChange('", ns("lastClick"), "', Math.random())
                                       });"))
                     )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' reportGraphicalScheduleModuleServer
#'
#' @param id the id
#' @param input the input
#' @param output the output
#' @param session the session
#' @param inSemester inSemester
#' @param theMasterCourses the master course list
#' @param theCombinedData the combined data
#' @param theLeaveData the leave data
#' @param theCourseInventory the course inventory
#' @param inSemesterCodes the semester codes
#'
#' @return
#' @export
#'
#' @examples
reportGraphicalScheduleModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                                theCombinedData, theLeaveData, theCourseInventory, inSemesterCodes){
  moduleServer(
    id,
    function(input, output, session){
      toReturn <- reactiveValues(
        trigger=NULL,
        mcData=NULL,
        afData=NULL,
        leaveData=NULL,
        combinedData=NULL,
        viewBeginSemester=NULL,
        viewEndSemester=NULL,
        oldFacultyName=NULL
      )
      #############################
      # Input Elements            #
      #############################
      output$chooseSemesterList1 <- renderUI({
        ns <- session$ns
        currentYear <- year(today())
        currentYearPlus5 <- currentYear+5
        useSemesters <- inSemesterCodes %>%
          filter(Year <= currentYearPlus5) %>%
          filter(as.character(current) >= "201431")
        chosen.semester <- NULL
        t.out <- div(id=ns("theSemester1"), class="semesterSelect",
                     selectInput(ns("theSemester1"), "Semester",
                                 useSemesters$semester.display, selected = chosen.semester)
        )
        t.out
      })

      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Graphical Schedule")
        theTitle
      })

      output$graphicalSchedule <- DT::renderDataTable({
        ns <- session$ns
        graphicalSchedule <- function(courseInventory, inSemesterCodes, mcData){
          req(courseInventory)
          req(mcData())
          #req(input$theSemester5)   #This needs to be changed to the correct input
          numClasses <- nrow(courseInventory)
          numYears <- 4
          focalSemester <- "Spring 2022"
          keepSemesters <- c("Spring 2021", "Summer 2021", "Fall 2021", "Spring 2022")

          focalSemesterIndex <- inSemesterCodes %>%
            filter(longSemester==focalSemester) %>%
            select("index") %>%
            unlist() %>%
            unique() %>%
            as.vector()
          justSemesters <- mcData() %>%
            select("semester") %>%
            unique() %>%
            unlist() %>%
            as.vector()

          midData <- NULL
          for(i in 1:length(justSemesters)){
            midData <- rbind(midData, data.frame(semester=justSemesters[i],
                                                 courseID=courseInventory$courseID))
          }

          midData <- midData %>%
            full_join(mcData(), by=c("semester", "courseID")) %>%
            mutate(OldDescription=Description) %>%
            select(-"Description") %>%
            left_join(courseInventory, by=c("courseID")) %>%
            filter(semester %in% keepSemesters) %>%
            left_join(inSemesterCodes, by=c("semester"="longSemester")) %>%
            mutate(buttonName=paste0("courseButton_", courseID, "_", sem4)) %>%
            mutate(instructor=case_when(!is.na(instructor) ~ paste0('<button type="button" class="btn btn-secondary courseButtonStyleCheck centerBlock" id=',ns(.$buttonName),'>', .$courseID, '</button>'),
                                        TRUE ~ paste0('<button type="button" class="btn btn-secondary courseButtonStyleUnselected centerBlock" id=', ns(.$buttonName),'>', .$courseID, '</button>'))) %>%
            select("courseID", "Description", "semester"="semester4", "instructor") %>%
            distinct() %>%
            arrange(semester) %>%
            pivot_wider(id_cols=c("courseID", "Description"), names_from=semester, values_from=instructor)

          columnSemesterNames <- names(midData)[3:6]
          outData <-  datatable(midData, escape=FALSE, selection="none", rownames=FALSE,
                                colnames=c("Course", "Description", columnSemesterNames),
                                options=list(dom='t', ordering=FALSE, pageLength=nrow(midData), scrollX=TRUE))
          outData
        }
        outData <- graphicalSchedule(theCourseInventory, inSemesterCodes, theMasterCourses)
      })

      #############################
      # Observers                 #
      #############################
      observeEvent(input$lastClick, {
        ns <- session$ns

        toggleClass(id=input$lastClickId,  class="courseButtonStyleUnselected", asis=TRUE)
        toggleClass(id=input$lastClickId, class="courseButtonStyleCheck", asis=TRUE)
        # Extract the courseID and the semester
        t.courseID <- gsub(".*[_]([^.]+)[_].*", "\\1", input$lastClickId)
        t.sem <- substr(input$lastClickId, nchar(input$lastClickId)-2, nchar(input$lastClickId))

        t.description <- theMasterCourses() %>%
          filter(courseID==t.courseID) %>%
          select("Description") %>%
          unique() %>%
          unlist() %>%
          as.character()

        toReturn$mcData <- alterMasterCourses(t.courseID, t.description, t.sem, theMasterCourses())

      })

      #############################
      # Helping functions         #
      #############################
      alterMasterCourses <- function(t.course, t.description, t.sem, t.mc){
        cat(blue("In alter master course\n"))
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()
        t.Dept <- substr(t.course, 1,4)

        if(t.course < 600){
          t.section <- 500
        } else t.section <- 600
        new.course <- data.frame(Department=t.Dept,
                                 Course=t.course,
                                 Description=t.description,
                                 semester=t.sem1,
                                 instructor="Unassigned",
                                 courseID=t.course,
                                 section=t.section,
                                 W=FALSE,
                                 C=FALSE,
                                 OL=FALSE,
                                 H=FALSE,
                                 SA=FALSE,
                                 stacked=FALSE,
                                 load.contribution=1)
        #print(names(new.course))
        #print(names(t.mc))
        t.1 <- new.course %>%
          anti_join(t.mc, by=c("courseID", "semester"))

        if(nrow(t.1)==0){
          t.mc <- t.mc %>%
            filter(!(courseID == new.course$courseID & semester == new.course$semester))

        } else {
          t.mc <- rbind(t.mc, new.course)
        }
        t.mc
      }

      output$graphicalScheduleSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("chooseSemesterList1"))
      })

      return(toReturn)
    }
  )
}
