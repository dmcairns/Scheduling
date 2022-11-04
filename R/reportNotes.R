#' reportNotesModuleUI
#'
#' @param id the id
#'
#' @return
#' @export
#'
#' @examples
reportNotesModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     h4("something"),
                     width=40,
                     background="#998542",
                     id=ns("notesSidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("notesReport")))
                                  )
                   ),
                   id=ns("notesReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' reportNotesModuleServer
#'
#' @param id the id
#' @param input the input
#' @param output the output
#' @param session the session
#' @param inSemester input semester
#' @param theNotes the notes
#' @param theCombinedData the combined data
#' @param allowUpdate allow updates logical
#' @param rds.path the rds path
#'
#' @return
#' @export
#'
#' @examples
reportNotesModuleServer <- function(id, input, output, session, inSemester, theNotes, theCombinedData,
                                    allowUpdate=FALSE, rds.path=NULL){
  moduleServer(
    id,
    function(input, output, session){
      toReturn <- reactiveValues(
        notes=NULL
      )
      cat(yellow("[reportNotesModuleServer]\n"))

      localSelectedFaculty <- NULL
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
      output$chooseFaculty <- renderUI({
        ns <- session$ns
        facultyList <- theCombinedData() %>%
          select("shortName") %>%
          unlist() %>%
          unique() %>%
          as.vector()

        selectInput(ns("theFacultyMember"), "Faculty Name:", choices=facultyList)
      })
      output$text <- renderPrint({
        theNotes()
      })
      observeEvent(input$theFacultyMember,{
        localSelectedFaculty <- input$theFacultyMember
      })
      observeEvent(input$addNote,{
        cat(yellow("Add Note Clicked", inSemester(), "\n"))

        observeEvent(input$submitAddNote, {

          theRevisedNotesData <- theNotes() %>%
            add_row(semester=input$noteSemester,
                    note=input$noteText,
                    source=input$noteSource)
          toReturn$notes <- theRevisedNotesData
          removeModal()
        })
        dataModal <- function() {
          ns <- session$ns


          modalDialog(
            h4("Add a scheduling note."),
            selectInput(ns("noteSemester"), label="Semester",
                        choices=as.character(unique(theCombinedData()$longSemester)),
                        selected=inSemester()),
            textInput(ns("noteText"), label="Note", placeholder="Add note text"),
            selectInput(ns("noteSource"), label="Source",
                        choices=c("e-mail", "SMS", "oral", "other"),
                        selected="e-mail"),
            textInput(ns("noteDate"), label="Date", placeholder=today()),


            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitAddNote"), "Add")
            ),
            size="s"
          )
        }
        showModal(dataModal())
      })
      observeEvent(input$deleteNote, {
        cat(yellow("Delete Note Clicked\n"))

        observeEvent(input$submitDeleteNote, {
          ns<- session$ns
          selectedRows <- input$deleteNoteDT_rows_selected
          notesIndices <- c(1:nrow(theNotes()))
          keepRows <- notesIndices[!(notesIndices %in% selectedRows)]
          theRevisedNotesData <- theNotes()[keepRows,]
          toReturn$notes <- theRevisedNotesData

        })

        output$deleteNoteDT <- renderDataTable(
          datatable(theNotes(), escape=FALSE, rownames=FALSE,
                    options=list(dom='t', ordering=FALSE, scrollX=TRUE))
        )
        dataModal <- function() {
          ns <- session$ns
          modalDialog(
            h4("Delete a scheduling note."),
            DTOutput(ns("deleteNoteDT")),

            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitDeleteNote"), "Delete")
            ),
            size="m"
          )
        }
        showModal(dataModal())
      })
      observeEvent(input$editNote, {
        cat(yellow("Edit Note Clicked\n"))
        ns <- session$ns
        proxy <- dataTableProxy(ns("editNoteDT"))
        revisedData <- theNotes()
        observeEvent(input$editNoteDT_cell_edit, {
          info = input$editNoteDT_cell_edit
          #str(info)
          i = info$row
          j = info$col + 1  # column index offset by 1
          v = info$value
          revisedData[i, j] <- DT::coerceValue(v, revisedData[i, j])
          replaceData(proxy, revisedData, resetPaging = FALSE, rownames = FALSE)
          toReturn$notes <- revisedData
        })
        observeEvent(input$submitEditNote, {
          ns<- session$ns
          removeModal()
        })

        output$editNoteDT <- renderDataTable(
          datatable(theNotes(), escape=FALSE, editable=TRUE, rownames=FALSE,
                    options=list(dom='t', ordering=FALSE, scrollX=TRUE))
        )
        dataModal <- function() {
          ns <- session$ns
          modalDialog(
            h4("Edit a scheduling note."),
            DTOutput(ns("editNoteDT")),

            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitEditNote"), "Submit")
            ),
            size="m"
          )
        }
        showModal(dataModal())
      })
      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Scheduling notes for ", inSemester())
        theTitle
      })
      output$notesReport <- renderUI({
        ns <- session$ns
        #req(inSemester())
        #req(theMasterCourses)
        t.out <- tagList(
          verbatimTextOutput(ns("text")),
          actionButton(ns("addNote"), "Add Note"),
          actionButton(ns("deleteNote"), "Delete Note"),
          actionButton(ns("editNote"), "Edit Note")
        )
        t.out
      })
      output$notesSidebar <- renderUI({
        ns <- session$ns
        #uiOutput(ns("chooseFaculty"))
      })

      outputOptions(output, "chooseFaculty", suspendWhenHidden = FALSE)

      return(toReturn)
    }
  )
}
