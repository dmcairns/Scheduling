reportUnassignedCoursesModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("legacyReportSidebar")),
                     width=40,
                     background="#998542",
                     id=ns("legacyReportSidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("legacyReport2")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

reportUnassignedCoursesModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                                theCombinedData, theLeaveData, inSemesterCodes, synchronize,
                                                chosenSemester=NULL, suspendWhenHidden){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      #cat(green("[reportUnassignedCoursesModuleServer]\n"))
      toReturn <- reactiveValues(
        trigger=NULL,
        mcData=NULL,
        afData=NULL,
        combinedData=NULL,
        viewSemester=NULL

      )

      #############################
      # Input Elements            #
      #############################
      output$chooseSemesterList1 <- renderUI({
        req(synchronize())
        req(chosenSemester())
        #cat(red("[unassignedCoursesModuleServer] input$synchSwitch:", synchronize(), "\n"))
        #cat(red("[unassignedCoursesModuleServer] chosenSemester:", chosenSemester(), "\n"))
        ns <- session$ns
        currentYear <- year(today())
        currentYearPlus5 <- currentYear+5
        useSemesters <- inSemesterCodes %>%
          filter(Year <= currentYearPlus5) %>%
          filter(as.character(current) >= "201431")
        chosen.semester <- NULL
        if(!is.null(synchronize())){
          #cat(red("[reportUnassignedCoursesModuleServer] synchronize() is not NULL\n"))
          if(synchronize()){
            chosen.semester <- chosenSemester()
          } else {
            chosen.semester <- toReturn$viewSemester
          }
        } else {
          chosen.semester <- "Fall 2019"
        }
        #cat(green("[reportUnassignedCoursesModuleServer] synchronize():", synchronize(), "\n"))
        #cat(green("[reportUnassignedCoursesModuleServer] toReturn$viewSemester:", toReturn$viewSemester, "\n"))
        t.out <- div(id=ns("theSemester1"), class="semesterSelect",
                     selectInput(ns("theSemester1"), "Semester",
                                 useSemesters$semester.display, selected = chosen.semester)
        )
        #cat(green("Leaving chooseSemesterList1\n"))
        t.out
      })

      #############################
      # Observers                 #
      #############################
      observeEvent(input$theSemester1,{
        toReturn$viewSemester <- input$theSemester1
        #cat(yellow("[reportUnassignedCoursesModule] toReturn$viewSemester:", toReturn$viewSemester, "\n"))
        #cat(yellow("[reportUnassignedCoursesModule] input$theSemestre1:", input$theSemester1, "\n"))
      })
      observeEvent(synchronize(),{
        ns <- session$ns
        #cat(green("[reportUnassignedCoursesModule] toggling inputElementHidden\n"))
        toggleCssClass(ns("inputElementWarning"), "inputElementHidden", asis=TRUE)
        toggleCssClass(ns("inputElementSemesterDiv"), "inputElementHidden", asis=TRUE)
      })


      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Unassigned Courses for ", input$theSemester1)
        theTitle
      })
      output$legacyReport2 <- renderUI({
        ns <- session$ns
        output$UnassignedTable <- DT::renderDataTable({
          unassigned.courses
        })
        output$WTable <- DT::renderDataTable({
          w.courses
        })
        #Display a list of the course without an instructor (instructor="Unassigned")
        #req(inSemester())
        req(input$theSemester1)
        unassigned.courses <- theMasterCourses() %>%
          #filter(semester==inSemester()) %>%
          filter(semester==input$theSemester1) %>%
          filter(instructor=="Unassigned") %>%
          arrange(courseID, section) %>%
          datatable(selection="none", rownames=FALSE, options = list(dom='t', ordering=FALSE, scrollX = T))

        #print(unassigned.courses)
        w.courses <- theMasterCourses() %>%
          #filter(semester==inSemester()) %>%
          filter(semester==input$theSemester1) %>%
          filter(W==TRUE) %>%
          arrange(courseID, section) %>%
          datatable(selection="none", rownames=FALSE, options = list(dom='t', ordering=FALSE, scrollX = T))

        tagList(
          br(),
          h3("Unassigned Courses"),
          div(class="aBoxBodyDTBackground",
              fluidRow(
                column(12,DT::dataTableOutput(ns("UnassignedTable"))))),

          #Display a list of Regional Courses Staffed
          #Display a list of Writing Courses Staffed
          br(),
          h3("Writing Courses Staffed"),
          div(class="aBoxBodyDTBackground",
              fluidRow(
                column(12, DT::dataTableOutput(ns("WTable")))))

          #Display a list of classes taught by GALs
          #    Need a list of full-time salaried faculty
        )

      })
      output$legacyReportSidebar <- renderUI({
        ns <- session$ns
        theList <- NULL
        theList <- tagList(
          div(
            id=ns("inputElementWarning"),
            class="inputElement inputElementHidden",
            p("The elements are synchronized.")
          ),
          div(
            id=ns("inputElementSemesterDiv"),
            class="inputElement inputElementHidden",
            uiOutput(ns("chooseSemesterList1"))
          )
        )
        theList
      })

      output$sem <- renderPrint({
        paste("[Report Module] inSemester:", inSemester())
      })


      outputOptions(output, "chooseSemesterList1", suspendWhenHidden = FALSE)
      outputOptions(output, "legacyReportSidebar", suspendWhenHidden = FALSE)
    }
  )
}
