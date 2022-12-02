#' courseListingUI
#'
#' @param id the id
#' @param label the label
#'
#' @return
#' @export
#'
#' @examples
courseListingUI <- function(id, label = "Course Listing"){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("courseListSidebar")),
                     width=40,
                     background="#998542",
                     id=ns("sidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("courseList")))
                                  )
                   ),
                   id=ns("courseListBox"),
                   class="schBoxTestOct"
                 ))
}

#' courseListingServer
#'
#' @param id The id
#' @param inSemesterCodes the inSemesterCodes
#' @param theMasterCourses the master courses
#' @param theFaculty the Faculty
#' @param theCoursesOffered the coursesOffered
#' @param theCombinedData theCombined Data
#' @param theCourseInventory the courseInventory
#' @param showDebugCues logical variable to show debug cues
#' @param synchronize synchronize across boxes
#' @param chosenSemester the semester chosen
#'
#' @return
#' @export
#'
#' @examples
courseListingServer <- function(id, inSemesterCodes, theMasterCourses, theFaculty,
                                theCoursesOffered, theCombinedData, theCourseInventory,
                                showDebugCues, synchronize,
                                chosenSemester=NULL){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #browser()
      ns <- session$ns
      vals <- reactiveValues()

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
        ns <- session$ns
        currentYear <- year(today())
        currentYearPlus5 <- currentYear+5
        useSemesters <- inSemesterCodes %>%
          filter(Year <= currentYearPlus5) %>%
          filter(as.character(current) >= "201431")
        chosen.semester <- NULL

        if(!is.null(synchronize)){
          if(synchronize()){
            chosen.semester <- chosenSemester()
          } else {
            chosen.semester <- toReturn$viewSemester
          }
        }
        t.out <- div(id=ns("theSemester1"), class="semesterSelect",
                     selectInput(ns("theSemester1"), "Semester",
                                 useSemesters$semester.display, selected = chosen.semester)
        )
        t.out
      })
      #############################
      # Observers                 #
      #############################
      observeEvent(input$lastClickClassOptions1, {
        ns <- session$ns
        addSection <- function(in.courseID, in.semester){
          cat(green("Add a section1:", in.semester, in.courseID, "\n"))
          showModal(modalDialog(
            id = "addSectionModal",
            h3("Insert a section"),
            uiOutput(ns("insertSectionDT")),
            title = "Insert Section",
            footer=list(actionButton(ns("insertSectionCommit"), "Commit"),
                        actionButton(ns("dismissSectionButton"), "Dismiss"))
          ))
          NULL
        }
        removeSection <- function(in.courseID, in.semester){
          ns <- session$ns
          cat(blue("Delete a section1:", in.semester, in.courseID, "\n"))
          showModal(modalDialog(
            id = "deleteSectionModal",
            h3("Delete section(s)"),
            uiOutput(ns("deleteSectionDT")),
            title = "Delete Section",
            footer=list(actionButton(ns("deleteSectionCommit"), "Commit"),
                        actionButton(ns("dismissSectionButton"), "Dismiss"))
          ))
          NULL
        }

        cat(red("Button pushed.  lastClickClassOptionsId:", input$lastClickClassOptionsId, "\n"))
        t.elements <- unlist(strsplit(input$lastClickClassOptionsId, "_"))
        t.courseID <- t.elements[2]
        t.sem <- t.elements[3]


        theSemester <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()

        current.value <- theMasterCourses() %>%
          filter(semester==theSemester) %>%
          group_by(courseID) %>%
          summarise(num.sections=n(), .groups="drop") %>%
          filter(courseID == t.courseID) %>%
          select("num.sections") %>%
          unlist() %>%
          as.numeric()

        if(length(current.value)==0)
          current.value <- 0


        if(as.numeric(input$lastClickClassOptionsValue) > current.value){
          addSection(t.courseID, t.sem)
        }
        if(as.numeric(input$lastClickClassOptionsValue) < current.value){
          removeSection(t.courseID, t.sem)
        }

      })
      observeEvent(input$insertSectionCommit, {
        # when this button is hit, add the new data into the master.course data.frame
        cat("In commit\n")
        numNewSections <- 1
        #localMasterCourses <- isolate(rv$master.courses)

        #localMasterCourses <- isolate(theMasterCourses())
        ###################################################
        # For each added section, need the following:     #
        #     Department                                  #
        #     Course                                      #
        #     Description                                 #
        #     semester                                    #
        #     instructor (short name)                     #
        #     courseID                                    #
        #     section                                     #
        #     W                                           #
        #     C                                           #
        #     OL                                          #
        #     H                                           #
        #     SA                                          #
        #                                                 #
        ###################################################

        localW <- FALSE
        localC <- FALSE
        localOL <- FALSE
        localH <- FALSE
        localSA <- FALSE

        t.elements <- unlist(strsplit(input$lastClickClassOptionsId, "_"))

        localCourseID <- t.elements[2]
        t.sem <- t.elements[3]
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()

        localDepartment <- substr(localCourseID, 1,4)
        localCourse <- substr(localCourseID, 5,7)

        localDescription <- theCourseInventory %>%
          filter(courseID==localCourseID) %>%
          select("Description") %>%
          unlist()
        localSemester <- t.sem1
        localInstructor <- input$instructorNew1
        localSection <- input$sectionNew1
        if(!is.null(input$courseCharacteristics1)){
          if ("W" %in% input$courseCharacteristics1) localW <- TRUE
          if ("C" %in% input$courseCharacteristics1) localC <- TRUE
          if ("OL" %in% input$courseCharacteristics1) localOL <- TRUE
          if ("H" %in% input$courseCharacteristics1) localH <- TRUE
          if ("SA" %in% input$courseCharacteristics1) localSA <- TRUE
        }

        cat(red("before add_row\n"))

        theRevisedMasterCourses <- theMasterCourses()
        nextRecNum <- max(as.numeric(theRevisedMasterCourses$recnum))+1
        # theRevisedMasterCourses <- theRevisedMasterCourses %>%
        #   mutate(nr=as.numeric(recnum))
#        theRevisedMasterCourses <- theMasterCourses() %>%
        theRevisedMasterCourses <- theRevisedMasterCourses %>%
          add_row(recnum = as.character(nextRecNum),
                  Department = as.character(localDepartment),
                  Course = as.character(localCourse),
                  Description = as.character(localDescription),
                  semester = as.character(localSemester),
                  instructor = as.character(localInstructor),
                  courseID = as.character(localCourseID),
                  section = bit64::as.integer64(localSection),
                  W = localW,
                  C = localC,
                  OL = localOL,
                  H = localH,
                  SA = localSA)

        cat("Added course to master.courses\n")
        cat(green("nrow(theMasterCourses):", nrow(theRevisedMasterCourses), "\n"))
        ############################################
        # Need to update the assigned loads     YYY   #
        ############################################

        t.assignedLoads <- theRevisedMasterCourses %>%
          group_by(instructor, semester) %>%
          summarise(assigned.load=sum(load.contribution), .groups="drop")

        theRevisedCombinedData <- theCombinedData() %>%
          #rv$combinedData <<- isolate(rv$combinedData) %>%
          left_join(t.assignedLoads, by=c("shortName"="instructor", "longSemester"="semester")) %>%
          mutate(assigned.load=assigned.load.y) %>%
          select("Faculty", "shortName", "shortNameNoSpace", "longSemester", "shortSemester", "numericSemester",
                 "sem2", "rank", "load", "assigned.load")


        # if there are no courses scheduled for localSemester, then the following will throw
        # an error.  need to add not only the course, but the semester to rv$offeredCourses.
        #if(is.null(rv$offeredCourses[localSemester][[1]])){


        ###################################################
        # The code below handles the ocData               #
        # which is currently not used in this             #
        # app.  Commented out to test what happens if it  #
        # is not done.                                    #
        ###################################################

        # theRevisedCoursesOffered <- theCoursesOffered()
        # browser()
        # if(is.null(theRevisedCoursesOffered[localSemester][[1]])){
        #   #create the dataframe for this.
        #   # Make sure that it is being added appropriately to rv$offeredCourses.
        #   #  Get this warning:
        #   # Warning in rv$offeredCourses[localSemester] <- tibble(Department = localDepartment,  :
        #   #   number of items to replace is not a multiple of replacement length
        #
        #   #rv$offeredCourses[localSemester] <- tibble(
        #   theRevisedCoursesOffered[[localSemester]] <- tibble(
        #     Department = localDepartment,
        #     Course=localCourse,
        #     Description = localDescription,
        #     semester = localSemester,
        #     instructor = localInstructor,
        #     courseID = localCourseID,
        #     section = localSection,
        #     W = localW,
        #     C = localC,
        #     OL = localOL,
        #     H = localH,
        #     SA = localSA
        #   )
        # } else {
        #   theRevisedCoursesOffered[[localSemester]] %>%
        #     add_row(Department = localDepartment,
        #             Course = localCourse,
        #             Description = localDescription,
        #             semester = localSemester,
        #             instructor = localInstructor,
        #             courseID = localCourseID,
        #             section = localSection,
        #             W = localW,
        #             C = localC,
        #             OL = localOL,
        #             H = localH,
        #             SA = localSA) %>%
        #     arrange(Department, courseID, section)
        # }

        toReturn$mcData <- theRevisedMasterCourses
        toReturn$combinedData <- theRevisedCombinedData
        # toReturn$ocData <- theRevisedCoursesOffered
        removeModal()
      })
      observeEvent(input$deleteSectionCommit, {
        #cat("In deleteSectionCommit\n")
        ns <- session$ns
        t.elements <- unlist(strsplit(input$lastClickClassOptionsId, "_"))

        localCourseID <- t.elements[2]
        t.sem <- t.elements[3]
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()
        # when this button is hit, delete sections from the master.course data.frame



        #cat(red("t.sem1:", t.sem1, "\n"))
        #cat("isolate(rv$tempSemester):", isolate(rv$tempSemester), "\n")
        #cat(red("localCourseID:", localCourseID, "\n"))
        #localSemester <- convertSemester2(isolate(rv$tempSemester))
        # create existing.sections locally

        #existing.sections <- isolate(rv$master.courses) %>%
        existing.sections <- theMasterCourses() %>%
          filter(semester == t.sem1) %>%
          filter(courseID == localCourseID)
        #print(existing.sections)
        localSemester <- t.sem1
        #numSections <- nrow(isolate(rv$existing.sections))
        numSections <- nrow(existing.sections)
        #numSections <- 1
        # cat("localCourseID:", localCourseID, "\n")
        # cat("localSemester:", localSemester, "\n")
        # cat("numSections:", numSections, "\n")
        # cat("names(input):", names(input), "\n")
        for(i in 1:numSections){
          if(i==1){
            t.1 <- (input[[(paste0("courseDeleteBox",i))]])
          } else {
            t.1 <- c(t.1, input[[(paste0("courseDeleteBox", i))]])
          }
        }
        #cat(red("t.1:"))
        #print(t.1)
        #cat("\n")
        #need.to.be.removed <- isolate(rv$existing.sections)[t.1,]
        need.to.be.removed <- existing.sections[t.1,]
        # assign("need.to.be.removed", need.to.be.removed, pos=1)
        # assign("t.ocData", isolate(theCoursesOffered()), pos=1)
        # assign("t.existingSections", existing.sections, pos=1)
        # assign("t.mcX", theMasterCourses(), pos=1)
        #print(need.to.be.removed)
        #cat("In deleteSectionCommit\n")

        # cat(yellow("[deleteSectionCommit]\n"))
        #rv$master.courses <- isolate(rv$master.courses) %>%
        theRevisedMasterCourses <- theMasterCourses() %>%
          anti_join(need.to.be.removed, by=c("semester", "courseID", "section"))

        theRevisedCoursesOffered <- theCoursesOffered()
        #rv$offeredCourses[[localSemester]] <- isolate(rv$offeredCourses)[[localSemester]] %>%

        # theRevisedCoursesOffered[[localSemester]] <- theRevisedCoursesOffered[[localSemester]] %>%
        #   anti_join(need.to.be.removed, by=c("semester", "courseID", "section"))


        toReturn$mcData <- theRevisedMasterCourses

        #toReturn$ocData <- theRevisedCoursesOffered
        #assign("t.mc1", rv$master.courses, pos=1)
        removeModal()

      })
      observeEvent(input$dismissSectionButton, {
        #cat("Dismiss button pushed\n")
        #attr(rv$master.courses, "dismissFlag") <- sample(1:1000000, 1)
        removeModal()
      })
      observeEvent(input$theSemester1,{
        if(is.null(synchronize())){
          toReturn$viewSemester <- input$theSemester1
        } else if (!synchronize()){
          toReturn$viewSemester <- input$theSemester1
        }
      })

      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Course Listing for ", input$theSemester1)
        theTitle
      })
      output$courseList <- renderUI({
        tagList(
          fluidRow(
            column(12, div(id=ns("classOptions"), class="classOptionsTable",
                           dataTableOutput(ns("classOptionsNewDT"))))
          ),
          tags$script(paste0("$(document).on('click', '.classOptionsTable input', function(){
                                           Shiny.onInputChange('", ns("lastClickClassOptionsId"), "',this.id);
                                           Shiny.onInputChange('", ns("lastClickClassOptionsValue"),"', this.value);
                                           Shiny.onInputChange('", ns("lastClickClassOptions1"), "', Math.random())
                                       });"))
        )
      })
      output$courseListSidebar <- renderUI({
        uiOutput(ns("chooseSemesterList1"))
      })
      output$classOptionsNewDT <- DT::renderDataTable({

        # theMasterCourses is a reactive variable passed into the module
        # theOfferedSections is a reactive variable passed into the module

        req(input$theSemester1)

        determineSingleSemesterOfferingsSections <- function(data, inSemester, course.inventory){

          req(inSemester)
          t.sections <- data() %>%
            right_join(course.inventory, by=c("courseID"))
          t.sections1 <- t.sections %>%
            filter(semester==inSemester) %>%
            group_by(courseID) %>%
            summarise(num.sections=n(), .groups="drop") %>%
            right_join(t.sections, by="courseID") %>%
            mutate(num.sections=case_when(
              is.na(num.sections) ~ 0.0,
              TRUE ~ as.double(num.sections))) %>%
            select("courseID", "num.sections", "Description"="Description.y") %>%
            unique()
          t.sections1
        }

        theSemester <- input$theSemester1
        sem.code <- inSemesterCodes %>%
          filter(longSemester==theSemester) %>%
          select(current.code) %>%
          unlist() %>%
          as.character()

        sections.offered <- determineSingleSemesterOfferingsSections(theMasterCourses,
                                                                     theSemester, theCourseInventory)

        numClasses <- dim(sections.offered)[[1]]
        sections.offered <- sections.offered %>%
          mutate(num.sects= paste0('<input type="number" id=sections',"_",.$courseID,'_',sem.code, ' value=', .$num.sections, ' step="1" >')) %>%
          select("courseID", "Description", "num.sects") %>%
          arrange(courseID)


        t.out <-     datatable(sections.offered,
                               selection="none", rownames=FALSE,
                               options=list(dom='t', ordering=FALSE, pageLength=100),
                               colnames=c("Course", "Title", "Sections"),
                               escape=FALSE)


        t.out
      })

      #############################
      # Modal Elements            #
      #############################
      output$insertSectionDT <- renderUI({
        ns <- session$ns
        t.elements <- unlist(strsplit(input$lastClickClassOptionsId, "_"))
        t.courseID <- t.elements[2]
        t.sem <- t.elements[3]
        courseNum <- as.numeric(substr(t.courseID, 5,7))

        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()

        numSections <- 1
        existing.sections <- theMasterCourses() %>%
          filter(semester==t.sem1) %>%
          filter(courseID==t.courseID) %>%
          select("section") %>%
          unlist()

        if (length(existing.sections)==0){
          #cat("There are no existing sections.\n")
          if(courseNum < 500) {
            new.sectionNum <- seq(500, 500+numSections)
            if(numSections==1) new.sectionNum <- 500
          } else {
            new.sectionNum <- seq(600, 600+numSections)
            if(numSections==1) new.sectionNum <- 600
          }
        } else {
          new.sectionNum <- seq(max(as.numeric(unlist(existing.sections)))+1, (max(as.numeric(unlist(existing.sections)))+numSections))
        }
# print(names(theFaculty()))
# browser()
        # instructorList <- theFaculty() %>%
        #   mutate(Faculty=factor(Faculty)) %>%
        #   gather_("semester", "code", names(.)[-1]) %>%
        #   mutate(Faculty=as.character(Faculty)) %>%
        #   left_join(inSemesterCodes, by=c("semester"="semester4")) %>%
        #   filter(semester == convertSemester(isolate(input$theSemester1), inSemesterCodes)) %>%
        #   filter(!is.na(code)) %>%
        #   select("Faculty") %>%
        #   arrange(Faculty) %>%
        #   unlist() %>%
        #   as.vector() %>%
        #   fix.names() %>%
        #   c("Unassigned")

        instructorList <- theFaculty() %>%
          pivot_longer(cols=!Faculty, names_to="semester",
                       values_to="code", values_drop_na=TRUE) %>%
          filter(semester != "recnum") %>%
          mutate(semester=toupper(semester)) %>%
          filter(semester==convertSemester(isolate(input$theSemester1), inSemesterCodes)) %>%
          select("Faculty") %>%
          unlist() %>%
          as.vector() %>%
          fix.names() %>%
          c("Unassigned")


        t.out <- lapply(1:(numSections), function(i) {
          list(
            fluidRow(div(id="insertModalRow",
                         column(2, textInput(ns(paste0("courseNew", i)), label=NULL, value=t.courseID)),
                         column(2, textInput(ns(paste0("sectionNew", i)), label=NULL, value=new.sectionNum[i])),
                         column(3, selectInput(ns(paste0("instructorNew", i)),
                                               label = NULL,
                                               choices=instructorList,
                                               selected="Unassigned"
                         )),
                         column(5, checkboxGroupInput(ns(paste0("courseCharacteristics", i)),
                                                      label=NULL,
                                                      inline=TRUE,
                                                      choices=c("W", "C", "H", "OL", "SA")))
                         # need to make the checkbox input pretty.  Should have labels above the boxes, not next to them.
                         # only have the labels appear once in the modal window
            ))
          )
        }) #end of lapply

      })
      output$deleteSectionDT <- renderUI({
        ns <- session$ns
        t.elements <- unlist(strsplit(input$lastClickClassOptionsId, "_"))

        t.courseID <- t.elements[2]
        t.sem <- t.elements[3]
        t.sem1 <- inSemesterCodes %>%
          filter(current.code==t.sem) %>%
          select(longSemester) %>%
          unlist() %>%
          as.character()
        existing.sections <- theMasterCourses() %>%
          filter(semester==t.sem1) %>%
          filter(courseID==t.courseID)

        numExistingSections <- nrow(existing.sections)

        if(numExistingSections > 0){
          t.out <- lapply(1:(numExistingSections), function(j) {
            list(
              fluidRow(
                column(1, checkboxInput(ns(paste0("courseDeleteBox", j)),
                                        label=NULL)),
                column(3, div(id=paste0("courseDelete", j), existing.sections[j, "courseID"], class="courseDeleteCourseID")),
                column(2, div(id=paste0("sectionDelete", j), existing.sections[j,"section"], class="courseDeleteSection")),
                column(3, div(id=paste0("instructorDelete", j), existing.sections[j,"instructor"], class="courseDeleteInstructor"))

              ))
          }) # reset the numericInput to the number of sections chosen for deletion.  probably needs to be done in the deleteSectionCommit routine.

        } else {

          #reset the numericInput back to existing value + 1 (should be 0)  it's called sectionsOptions1 or 2
          # input$sectionsOptions1 <- 1  Doesn't work because input$sectionsOptions1 is read only.
          #what is the name of the div that input$sectionsOptions is held within?  .1.2019C
          #replace that element (removeUI).  Is it still responsive to the observeEvent or does a new observe need to be created?
          # insertUI is the opposite.
          # divName <- paste0("#courseSelectorColumn.", isolate(rv$tempIndex), ".", convertSemester(isolate(input$theSemester1)))
          # removeUI(selector=divName, immediate=TRUE)
          # removeUI(selector="#sectionsOptions2.2019C", immediate=TRUE)
          #cat("tempIndex:", rv$tempIndex, "divName:", divName, "\n")
          t.out <- "Can't have negative number of sessions!"
        }
        t.out
      })

      #############################
      # Helper Functions          #
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
      fix.names <- function(data){
        check.duplicate.names <- function(in.data){
          t.substr <- gsub(",.*$", "", in.data)
          t.first <- gsub('.*,\\s*','', in.data)
          t.first <- substr(t.first, 1,1)

          if((t.substr=="Bednarz")|(t.substr=="Guneralp"))
            t.out <- paste0(t.first, ". ", t.substr)
          else
            t.out <- t.substr
          t.out
        }
        t.out <- lapply(data, check.duplicate.names)
        t.out
      }
      #############################
      # Setup the output data     #
      #############################

      observe({vals$masterCourses <- theMasterCourses})
      outputOptions(output, "chooseSemesterList1", suspendWhenHidden = FALSE)
      outputOptions(output, "courseListSidebar", suspendWhenHidden = FALSE)
      return(toReturn)
    }
  )
}
