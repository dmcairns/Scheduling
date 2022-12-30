#' instructorAssignmentModuleUI
#'
#' @param id The id
#'
#' @return
#' @export
#'
#' @examples
instructorAssignmentModuleUI <- function(id){
  ns <- NS(id)
  useShinyjs()
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("sidebar")),
                     width=40,
                     background="#998542",
                     id=ns("sidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("instructorAssignment")))
                                  )
                   ),
                   id=ns("instructorAssignmentBox"),
                   class="schBoxTestOct"
                 ))

}

#' instructorAssignmentModuleServer
#'
#' @param id The id
#' @param input The input
#' @param output The output
#' @param session The session
#' @param inSemester The input semester
#' @param theMasterCourses The master course list
#' @param theCombinedData The combined data
#' @param theLeaveData The leave data
#' @param inSemesterCodes the semester codes
#' @param synchronize synchronize boxes
#' @param chosenSemester the chosen semester
#' @param suspendWhenHidden suspendWhenHidden
#' @param allowUpdate Allow for updates
#' @param rds.path the rds path
#'
#' @return
#' @export
#'
#' @examples
instructorAssignmentModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                             theCombinedData, theLeaveData, inSemesterCodes,
                                             synchronize,
                                             chosenSemester=NULL, suspendWhenHidden,
                                             allowUpdate=FALSE, rds.path=NULL){
  moduleServer(
    id,
    function(input, output, session){
      toReturn <- reactiveValues(
        trigger=NULL,
        mcData=NULL,
        afData=NULL,
        combinedData=NULL,
        viewSemester=NULL
      )
      #cat(red("[instructorAssignmentModuleServer] synchronize():", synchronize, "\n"))
      #############################
      # Input Elements            #
      #############################

      output$chooseSemesterList1 <- renderUI({
        req(synchronize())
        req(chosenSemester())
        #cat("In chooseSemesterList1\n")
        ns <- session$ns
        currentYear <- year(today())
        currentYearPlus5 <- currentYear+5
        useSemesters <- inSemesterCodes %>%
          filter(Year <= currentYearPlus5) %>%
          filter(as.character(current) >= "201431")
        chosen.semester <- NULL
        if(!is.null(synchronize())){
          if(synchronize()){
            chosen.semester <- chosenSemester()
          } else {
            chosen.semester <- toReturn$viewSemester
          }
        }
        #cat(blue("[instructorAssignmentModuleServer] synchronize():", synchronize(), "\n"))
        #cat(blue("[instructorAssignmentModuleServer] toReturn$viewSemester:", toReturn$viewSemester, "\n"))
        t.out <- div(id=ns("theSemester1"), class="semesterSelect",
                     selectInput(ns("theSemester1"), "Semester",
                                 useSemesters$semester.display, selected = chosen.semester)
        )
        #cat(green("Leaving chooseSemesterList1\n"))
        t.out
      })
      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Instructor Assignment for ", input$theSemester1)
        theTitle
      })
      output$sidebar <- renderUI({
        ns <- session$ns
        tagList(
          # div(id="divIDInModule", class="interactiveSelectContainerDiv",
          #     tagList(
          #       h4("test in input container"),
          #       selectInput("sidebarSelectInModule", "Test", choices=c(1,2,3), selected=2)
          #     )
          # ),
          uiOutput(ns("chooseSemesterList1")),

        )
      })
      output$instructorAssignment <- renderUI({
        ns <- session$ns
        tagList(
          div(id=ns("instructorAssignment"), class="instructorAssignmentTable",
              dataTableOutput(ns("scheduledClassesNewDT"))
          ),

          tags$script(paste0("$(document).on('change', '.interactiveSelectContainerDiv1 select', function(){
                       Shiny.setInputValue('", ns("instructorNameID"),"', this.id, {priority:'event'});
                       Shiny.setInputValue('", ns("changedInstructorNameValue"), "', this.value, {priority:'event'});
                      });")),

          tags$script(paste0("$(document).on('click', '.instructorAssignmentTable input', function(){
                                           Shiny.onInputChange('", ns("lastClickInputId"), "',this.id);
                                           Shiny.onInputChange('", ns("lastClickInput1"), "', Math.random())
                                       });"))
        )
      })
      output$scheduledClassesNewDT <- DT::renderDataTable({
        ns <- session$ns
        req(inSemester())
        req(theMasterCourses())
        req(theCombinedData())

        theSemester <- inSemester()

        dataForSemesterOfInterest <- theMasterCourses() %>%
          filter(semester == theSemester) %>%
          arrange(courseID, section)

        sem.code <- inSemesterCodes %>%
          filter(longSemester==theSemester) %>%
          select(current.code) %>%
          unlist() %>%
          as.character()
        t.df <- dataForSemesterOfInterest

        theInstructorsNew <- theCombinedData() %>%
          filter(shortSemester==convertSemester(theSemester)) %>%
          filter(!is.na(rank)) %>%
          select(displayName) %>%
          add_row(displayName="Unassigned") %>%
          unlist() %>%
          as.character() %>%
          sort()
        t.df <- t.df %>%
          mutate(OL1=case_when(
            (.[["OL"]]==TRUE) ~ paste0('<input type="checkbox" id=OL',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$OL, ' checked=checked>'),
            TRUE ~ paste0('<input type="checkbox" id=OL',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$OL, '>'))) %>%
          mutate(W1=case_when(
            (.[["W"]]==TRUE) ~ paste0('<input type="checkbox" id=W',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$W, ' checked=checked>'),
            TRUE ~ paste0('<input type="checkbox" id=W',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$W, '>'))) %>%
          mutate(C1=case_when(
            (.[["C"]]==TRUE) ~ paste0('<input type="checkbox" id=C',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$C, ' checked=checked>'),
            TRUE ~ paste0('<input type="checkbox" id=C',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$C, '>'))) %>%
          mutate(H1=case_when(
            (.[["H"]]==TRUE) ~ paste0('<input type="checkbox" id=H',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$H, ' checked=checked>'),
            TRUE ~ paste0('<input type="checkbox" id=H',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$H, '>'))) %>%
          mutate(SA1=case_when(
            (.[["SA"]]==TRUE) ~ paste0('<input type="checkbox" id=SA',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$SA, ' checked=checked>'),
            TRUE ~ paste0('<input type="checkbox" id=SA',"_",.$courseID,'_', .$section,"_",sem.code, ' value=', .$SA, '>'))) %>%
          mutate(instructorSelector="temp") %>%
          select("courseID", "section", "Description", "instructor", "OL1", "W1", "C1", "H1", "SA1", "instructorSelector")

        for(i in 1:dim(t.df)[1]){
          t.df$instructorSelector[i] <- as.character(tagList(
            div(id=paste0("divID_",i), class="interactiveSelectContainerDiv1",
                selectInput(paste0("inst_", t.df$courseID[i], "_", t.df$section[i], "_", sem.code),
                            label=NULL, choices = theInstructorsNew, selected=t.df$instructor[i], width=100)
            ))
          )
        }

        t.df <- t.df %>%
          select("courseID", "section", "Description", "instructorSelector", "OL1", "W1", "C1", "H1", "SA1")

        datatable(t.df,
                  selection="none", rownames=FALSE,
                  options=list(dom='t', ordering=FALSE, pageLength=100, scrollX=TRUE),
                  colnames=c("Course", "Section", "Description", "Instructor", "OL", "W", "C", "H", "SA"),
                  escape=FALSE)
      })

      #############################
      # Observers                 #
      #############################
      observeEvent(input$changedInstructorNameValue, {

        t.courseID <- substr(input$instructorNameID, 6,12)
        t.section <- gsub(".*[_]([^.]+)[_].*", "\\1", input$instructorNameID)
        t.sem <- substr(input$instructorNameID, nchar(input$instructorNameID)-2, nchar(input$instructorNameID))

        theRevisedMasterCourses <- updateInstructor(t.courseID, t.section, t.sem, theMasterCourses(), input$changedInstructorNameValue)

        t.assignedLoads <- theRevisedMasterCourses %>%
          group_by(instructor, semester) %>%
          summarise(assigned.load=sum(load.contribution), .groups="drop")

        t.printLoads <- t.assignedLoads %>%
          filter(semester==inSemester())

        theCombinedData <- theCombinedData() %>%
          left_join(t.assignedLoads, by=c("shortName"="instructor", "longSemester"="semester")) %>%
          mutate(assigned.load=assigned.load.y) %>%
          select("recnum", "Faculty", "shortName", "shortNameNoSpace", "longSemester", "shortSemester", "numericSemester",
                 "sem2", "rank", "load", "assigned.load", "displayName", "UIN")
        toReturn$mcData <- theRevisedMasterCourses
        toReturn$combinedData <- theCombinedData

      })
      observeEvent(input$lastClickLabelTrigger, {
        cat("input$lastClickLabelValue:", input$lastClickLabelValue, "\n")
      })
      observeEvent(input$lastClickInput1, {
        cat(blue(paste0("[observeEvent$lastClickInput] input$lastClickInputId:", input$lastClickInputId,"\n")))
        cat(blue(paste0("[observeEvent$lastClickInput] input$instructorNameID:", input$instructorNameID, "\n")))
        cat(blue(paste0("[observeEvent$lastClickInput] input$changedInstructorNameValue:", input$changedInstructorNameValue, "\n")))

        t.elements <- unlist(strsplit(input$lastClickInputId, "_"))
        t.var <- t.elements[1]
        t.courseID <- t.elements[2]
        t.section <- t.elements[3]
        t.sem <- t.elements[4]
        theRevisedMasterCourses <- updateAttributes(t.var, t.courseID, t.section, t.sem, theMasterCourses())
        toReturn$mcData <- theRevisedMasterCourses
      })

      #############################
      # Modal Elements            #
      #############################

      # There are no modal elements for this module

      #############################
      # Utility Functions         #
      #############################
      convertSemester <- function(inputSemester, semesterTable=inSemesterCodes){
        #   ############################################
        #   # converts a semester from display format  #
        #   # (Fall 2018) to 4code format (2018C)      #
        #   ############################################

        outputSemester <- semesterTable %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          mutate(sem4=paste0("20", current.code)) %>%
          filter(longSemester==inputSemester) %>%
          select(sem4) %>%
          as.character() %>%
          unlist()
        outputSemester
      }
      updateInstructor <- function(t.courseID, t.section, t.sem, t.mc, new.instructor){
        #cat(red("Enterred updateInstructor\n"))
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()

        if(new.instructor=="Unassigned"){
          new.UIN <- "-9999"
        } else {
          new.UIN <- theCombinedData() %>%
            filter(displayName==new.instructor) %>%
            pull(UIN) %>%
            unique()
        }


        t.mc <- t.mc %>%
          mutate(instructor = case_when((courseID==t.courseID) & (semester==t.sem1) & (section==t.section) ~ new.instructor,
                                        TRUE ~ instructor)) %>%
          mutate(Faculty = case_when((courseID==t.courseID) & (semester==t.sem1) & (section==t.section) ~ new.instructor,
                                     TRUE ~Faculty)) %>%
          mutate(UIN = case_when((courseID==t.courseID) & (semester==t.sem1) & (section==t.section) ~ new.UIN,
                                 TRUE ~ UIN))

        #cat(green("Leaving updateInstructor\n"))
        t.mc

      }
      updateAttributes <- function(t.variable, t.courseID, t.section, t.sem, t.mc){
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()

        t.mc <- t.mc %>%
          mutate(!!t.variable := case_when((courseID==t.courseID) & (semester==t.sem1) & (section==t.section) ~ !.[[t.variable]],
                                           TRUE ~ .[[t.variable]]))

        t.mc

      }


      outputOptions(output, "chooseSemesterList1", suspendWhenHidden = FALSE)
      outputOptions(output, "sidebar", suspendWhenHidden = FALSE)
      return(toReturn)
    }
  )
}
