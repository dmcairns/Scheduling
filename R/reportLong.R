#' reportLongModuleUI
#'
#' @param id the id
#'
#' @return
#' @export
#'
#' @examples
reportLongModuleUI <- function(id){
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
                                    column(12, uiOutput(ns("longReport")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' reportLongModuleServer
#'
#' @param id the id
#' @param input the input
#' @param output the output
#' @param session the session
#' @param inSemester semester passed in
#' @param theMasterCourses the master list of courses
#' @param theCombinedData the combined Data
#' @param theLeaveData the leave data
#' @param inSemesterCodes all semeser codes
#'
#' @return
#' @export
#'
#' @examples
reportLongModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                   theCombinedData, theLeaveData, inSemesterCodes){
  moduleServer(
    id,
    function(input, output, session){
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
        theTitle <- paste0("Long Report for ")
        theTitle
      })
      output$longReport <- renderUI({
        ns <-session$ns
        output$byCourse <- DT::renderDataTable({
          t.out <- rv$master.courses %>%
            filter(semester==input$theSemester9) %>%
            select("courseID", "section", "instructor") %>%
            arrange(courseID)
          #assign("t.out2", t.out, pos=1)
          t.out
        })
        t.table <- semester.codes
        DT::dataTableOutput(ns("byCourse"))

      })
      output$longReportDownload <- renderUI({
        ns <- session$ns
        tagList(
          br(),
          br(),
          downloadButton(ns("reportDataFrame"), "Download Report (MS Word)")
        )

      })
      output$reportDataFrame <- downloadHandler(

        #####################################################
        # This is a simple report that produces a PDF file  #
        # It does require that one piece of information is  #
        # passed into the Markdown file (params).           #
        # params is a dataframe                             #
        #####################################################


        # For PDF output, change this to "report.pdf"
        filename = "longReport.doc",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          #require(xtable)
          tempReport <- file.path(tempdir(), "reportDataFrame.Rmd")
          file.copy("reportDataFrame.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params1 <- isolate(rv$master.courses) %>%
            filter(semester==input$theSemester9) %>%
            select("courseID", "section", "instructor") %>%
            arrange(courseID)

          cat("dim(params1):", dim(params1), "\n")

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).

          rmarkdown::render(tempReport, output_file = file,
                            params = params1,
                            envir = new.env(parent = globalenv())
          )
        }
      )

      output$legacyReportSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("chooseSemesterList1"))
      })


    }
  )
}
