#' instructorLoadModuleUI
#'
#' @param id The id
#'
#' @return
#' @export
#'
#' @examples
instructorLoadModuleUI <- function(id){
  ns <- NS(id)
  useShinyjs()
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=FALSE,
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
                                    column(12, uiOutput(ns("instructorLoad")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' instructorLoadModuleServer
#'
#' @param id The id
#' @param input The input
#' @param output The output
#' @param session The session
#' @param inSemester The input semester
#' @param theMasterCourses the master course list
#' @param theCombinedData the combined data
#' @param theLeaveData the leave data
#' @param theAvailableFacultyData the available faculty data
#' @param inSemesterCodes insemester codes
#' @param allowUpdate allow update
#' @param rds.path rds path
#' @param facultyUINs faculty UINs
#'
#' @return
#' @export
#'
#' @examples
instructorLoadModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                       theCombinedData, theLeaveData, theAvailableFacultyData,
                                       inSemesterCodes, allowUpdate=FALSE, rds.path=NULL,
                                       facultyUINs=NULL){
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
        oldFacultyName=NULL,
        facultyUIN=NULL
      )
      gridSemestersDisplayed <- 6
      oldFacultyName <- NULL

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
      output$chooseSemesterList10BeginNew <- renderUI({
        #############################################
        # Logic:                                    #
        #   1) The default value at startup will    #
        #      be NULL.                             #
        #   2) If rv$availableFacultySemesterBegin  #
        #      is NULL, set it to current semester  #
        #   2) The value may not be after the value #
        #      of the ending semester unless the    #
        #      ending semester value == NULL        #
        #############################################
        req(inSemester())
        ns <- session$ns
        #chosenSemester <- inSemester()
        chosenSemester <- toReturn$viewBeginSemester
        if (is.null(chosenSemester)) {
          numericCurrentSemester <- CurrentSemester(numeric=TRUE)
          chosenSemester <- inSemesterCodes %>%
            filter(current == as.character(numericCurrentSemester)) %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            select(longSemester) %>%
            unlist() %>%
            as.character()

        }
        chosenSemesterIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester==chosenSemester) %>%
          select(index) %>%
          unlist()

        semestersToDisplay <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          select(longSemester) %>%
          unlist() %>%
          as.vector()

        #toReturn$viewBeginSemester <- chosenSemester
        t.out <- selectInput(ns("theSemester10BeginNew"), "First Semester",
                             choices=semestersToDisplay, selected = chosenSemester)  #can't add class="semInput".  Not allowed to specify class.
      })
      output$chooseSemesterList10EndNew <- renderUI({
        ##############################################
        # Logic:                                     #
        #   1) The default value at startup will     #
        #      be NULL.                              #
        #   2) If rv$availableFacultySemesterEnd     #
        #      is NULL, set it to current semester   #
        #      plus 2                                #
        #   2) The value may not be before the value #
        #      of the beginning semester unless      #
        #      the beginning semester value == NULL  #
        ##############################################
        ns <- session$ns
        req(inSemester())
        req(input$theSemester10BeginNew)

        #chosenSemester <- inSemester()
        chosenSemester <- toReturn$viewEndSemester

        #################################################
        # if chosenSemester is NULL, then set it to the #
        # current semester.                             #
        #################################################
        if (is.null(chosenSemester)) {
          numericCurrentSemester <- CurrentSemester(numeric=TRUE)
          chosenSemester <- inSemesterCodes %>%
            filter(current == as.character(numericCurrentSemester)) %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            select(longSemester) %>%
            unlist() %>%
            as.character()
        }

        #################################################
        # Because chosenSemester now has a value, find  #
        # its index (chosenSemesterIndex).              #
        # remember that this is for the ending semester #
        #################################################
        chosenSemesterIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester==chosenSemester) %>%
          pull(index)
          # select(index) %>%
          # unlist()

        #################################################
        # Now find the index value of the beginning     #
        # semester.                                     #
        # remember that this is for the ending semester #
        #################################################

        if(is.null(input$theSemester10BeginNew)){
          beginSemesterIndex <- chosenSemesterIndex
        } else {
          beginSemesterIndex <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            filter(longSemester==input$theSemester10BeginNew) %>%
            pull(index)
            # select(index) %>%
            # unlist()
        }


        endSemesterIndex <- beginSemesterIndex + gridSemestersDisplayed - 1

        semestersToDisplay <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(index >= beginSemesterIndex) %>%
          filter(index <= endSemesterIndex) %>%
          select(longSemester) %>%
          unlist() %>%
          as.vector()
        t.out <- selectInput(ns("theSemester10EndNew"), "End Semester",
                             choices=semestersToDisplay, selected = chosenSemester)
      })

      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Instructor Loads")
        theTitle
      })
      output$instructorLoad <- renderUI({
        ns <- session$ns
        tagList(
          fluidRow(id="splitScreen1",
                   column(12, div(id=ns("instructorLoads"), class="instructorLoadsTable",
                                  tagList(
                                    fluidRow(
                                      tagList(
                                        column(1, div(actionButton(ns("displayOneSemesterEarlier"), label=NULL, icon=icon("chevron-left"), class="SubtractOneSemester"),
                                                      class="HoldsNavButtonLeftSide")),
                                        column(10, uiOutput(ns("availableFacultyGridNewDT"))),
                                        column(1, div(actionButton(ns("displayOneSemesterLater"), label=NULL, icon=icon("chevron-right"), class="AddOneSemester"),
                                                      class="HoldsNavButtonRightSide"))
                                      )
                                    ),
                                    fluidRow(
                                      column(1),
                                      column(10, id="facultyModifyButtonsRow", actionButton(ns("facultyAddButton"), label=NULL, icon=icon("plus"), class="facultyAddButton"),
                                             actionButton(ns("facultyRemoveButton"), label=NULL, icon=icon("minus"), class="facultyRemoveButton")),
                                      column(1)
                                    )
                                  )
                   )
                   )
          ),
          tags$script(paste0("$(document).on('click', '.instructorLoadsTable td div', function(){
                                           Shiny.onInputChange('", ns("lastClickAvailableFacultyId"), "',this.id);
                                           Shiny.onInputChange('", ns("lastClickAvailableFacultyValue"),"', this.value);
                                           Shiny.onInputChange('", ns("lastClickAvailableFaculty1"), "', Math.random())
                                       });")),
          tags$script(paste0("$(document).on('click', '.instructorLoadsTable td a', function(){
                                           Shiny.onInputChange('", ns("lastClickAvailableFacultyNameId"), "',this.id);
                                           Shiny.onInputChange('", ns("lastClickAvailableFacultyNameValue"),"', this.value);
                                           Shiny.onInputChange('", ns("lastClickAvailableFacultyName1"), "', Math.random())
                                       });"))
        )
      })
      output$availableFacultyGridNewDT <- renderUI({
        ns <- session$ns

        t.out <- NULL
        req(inSemester())
        req(input$theSemester10BeginNew)
        req(input$theSemester10EndNew)


        ##########################################
        # Prepare the available.faculty data     #
        ##########################################

        if(!is.null(inSemester())){
          semester.codes <- inSemesterCodes %>%
            mutate(semester.display=paste(semester.chr, Year)) %>%
            mutate(semester4=paste0("20", current.code))

          targetBegin <- inSemesterCodes %>%
            filter(semester.display==input$theSemester10BeginNew) %>%
            select("semester4") %>%
            unlist()
          targetEnd <- inSemesterCodes %>%
            filter(semester.display==input$theSemester10EndNew) %>%
            select("semester4") %>%
            unlist()


          reducedFacultyData <- theAvailableFacultyData() %>%
            pivot_longer(cols=!Faculty, names_to="semester",
                         values_to="code", values_drop_na=TRUE) %>%
            filter(semester != "recnum") %>%
            mutate(semester=toupper(semester)) %>%
            left_join(inSemesterCodes, by=c("semester"="semester4")) %>%
            filter(semester >= targetBegin) %>%
            filter(semester <= targetEnd) %>%
            arrange(Faculty) %>%
            pull() %>%
            fix.names() %>%
            c("Unassigned")
        }


        ##########################################
        # Build the display table                #
        ##########################################

        if(!is.null(input$theSemester10BeginNew)){

          numRows <- length(unique(reducedFacultyData$Faculty))
          numCols <- length(unique(reducedFacultyData$semester))+1   #The +1 is for the faculty name column
          t.out <- tagList(
            dataTableOutput(ns("buildGridNewDTCombined"))
          )

        }



      })
      output$legacyReportSidebar <- renderUI({
        ns <- session$ns
        tagList(
          uiOutput(ns("chooseSemesterList1")),
          uiOutput(ns("chooseSemesterList10BeginNew")),
          uiOutput(ns("chooseSemesterList10EndNew"))
        )

      })
      output$buildGridNewDTCombined <- DT::renderDataTable({

        req(inSemester())
        req(input$theSemester10BeginNew)
        req(input$theSemester10EndNew)

        requireNamespace("crayon")
        #################################################
        # Requires that the following data be available #
        # within the calling environment:               #
        #                                               #
        #   numRows                                     #
        #   numCols                                     #
        #   reducedFacultyData                          #
        #################################################

        #browser()

        # fix the width of each column
        # Rotate column titles for semesters
        #
        # Modal automatically submits if deleting two people in one session.
        # Strange loads showing up for people in earlier semesters when they were not employed.
        # Difficulty in changing employment status to Not Employed.
        # How to handle changing a generic VAP to an existing person.  Need to update the course assignments.
        # Fix year arrow buttons so they work

        firstSemester <- input$theSemester10BeginNew
        lastSemester <- input$theSemester10EndNew
        firstSemesterIndex <- inSemesterCodes %>%
          filter(longSemester==firstSemester) %>%
          select(current) %>%
          pull(current)

        lastSemesterIndex <- inSemesterCodes %>%
          filter(longSemester==lastSemester) %>%
          select(current) %>%
          pull(current)
cat("something\n")
        loadTableData <- theCombinedData() %>%
          filter(numericSemester >= firstSemesterIndex) %>%
          filter(numericSemester <= lastSemesterIndex) %>%
          filter(!is.na(rank)) %>%
          arrange(Faculty) %>%
          mutate(FacultyHTML=paste0("<a id='load_", .$shortNameNoSpace,"', class=FN_Hyperlink>", Faculty, "</a>")) %>%
          mutate(loadHTML=case_when(is.na(rank) == TRUE ~ "Test",
                                    TRUE ~ paste0("<div id='load_", .$shortNameNoSpace, "_",.$sem2, "', class=positionCode",.$rank,">", .$load, "</div>"))) %>%
          select("Faculty"="FacultyHTML", "shortSemester", "load"="loadHTML") %>%
          pivot_wider(names_from=shortSemester, values_from=load)

        datatable(loadTableData,
                  selection="none", rownames=FALSE,
                  options=list(dom='t', ordering=FALSE, pageLength=100),
                  escape=FALSE)
      })

      #############################
      # Observers                 #
      #############################
      observeEvent(input$displayOneSemesterEarlier,{

        # determine the number of semesters displayed
        # if it is the maximum number, then need to first
        # shift the endSemester back one semester, then
        # shift the startSemester back one semester.


        currentIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester == input$theSemester10BeginNew) %>%
          pull(index)
          # select(index) %>%
          # unlist()
        endIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester == input$theSemester10EndNew) %>%
          pull(index)
          # select(index) %>%
          # unlist()
        oneSemesterEarlierIndex <- currentIndex-1
        oneSemesterEarlierLongSemester <- inSemesterCodes %>%
          filter(index==oneSemesterEarlierIndex) %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          pull(longSemester)
          # select(longSemester) %>%
          # unlist()
        numSemestersDisplayed <- endIndex-currentIndex+1
        #cat("oneSemesterEarlierLongSemester:", oneSemesterEarlierLongSemester, "numSemestersDisplayed:", numSemestersDisplayed, "\n")
        #cat("rv$numSemestersDisplayed:", isolate(rv$numSemestersDisplayed), "gridSemesters Displayed:", gridSemestersDisplayed, "\n")
        if(!is.null(numSemestersDisplayed)){
          if((numSemestersDisplayed+1) > gridSemestersDisplayed) {
            # Need to remove a semester from the right side first, then update the leftside
            currentLastIndex <- currentIndex + numSemestersDisplayed - 1
            newLastLongSemester <- inSemesterCodes %>%
              filter(index==(currentLastIndex-1)) %>%
              mutate(longSemester=paste(semester.chr, Year)) %>%
              pull(longSemester)
              # select(longSemester) %>%
              # unlist()
            toReturn$viewEndSemester <- newLastLongSemester
            toReturn$viewBeginSemester <- oneSemesterEarlierLongSemester
          } else{
            toReturn$viewBeginSemester <- oneSemesterEarlierLongSemester
          }
        }
        #cat("Leaving observeEvent for input$displayOneSemesterEarlier.\n ")
      }, ignoreInit=TRUE, once=FALSE)
      observeEvent(input$displayOneSemesterLater,{
        #cat("Entering observeEvent for input$displayOneSemesterLater.\n ")
        #cat("right arrow pushed\n")

        currentIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester == input$theSemester10EndNew) %>%
          pull(index)
          # select(index) %>%
          # unlist()
        oneSemesterLaterIndex <- currentIndex+1
        oneSemesterLaterLongSemester <- inSemesterCodes %>%
          filter(index==oneSemesterLaterIndex) %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          pull(longSemester)
          # select(longSemester) %>%
          # unlist()
        endIndex <- inSemesterCodes %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          filter(longSemester == input$theSemester10EndNew) %>%
          pull(index)
          # select(index) %>%
          # unlist()
        numSemestersDisplayed <- endIndex-currentIndex+1
        currentBeginIndex <- currentIndex - numSemestersDisplayed + 1

        if(!is.null(numSemestersDisplayed)){
          if((numSemestersDisplayed+1) > gridSemestersDisplayed) {
            # Need to remove a semester from the left side first, then update the right side
            currentBeginIndex <- currentIndex - numSemestersDisplayed + 2
            newFirstLongSemester <- inSemesterCodes %>%
              filter(index==(currentBeginIndex)) %>%
              mutate(longSemester=paste(semester.chr, Year)) %>%
              pull(longSemester)
              # select(longSemester) %>%
              # unlist()
            toReturn$viewBeginSemester <- newFirstLongSemester
            toReturn$viewEndSemester <- oneSemesterLaterLongSemester
            #rv$availableFacultySemesterBeginNew <- newFirstLongSemester
            #rv$availableFacultySemesterEndNew <- oneSemesterLaterLongSemester
          } else{
            toReturn$viewEndSemester <- oneSemesterLaterLongSemester
            #rv$availableFacultySemesterEndNew <- oneSemesterLaterLongSemester
          }
        }
        #cat("Updated  ", currentBeginIndex, "   ", oneSemesterLaterIndex, "\n")
        #cat("Leaving observeEvent for input$displayOneSemesterLater.\n ")
      }, ignoreInit=TRUE, once=FALSE)

      observeEvent(input$facultyRemoveButton,{
        observeEvent(input$submitRemoveFaculty,{
          #cat("This will remove", input$selectRemoveFaculty, "\n")
          observeEvent(input$confirmRemoveFaculty, {
            theRevisedCombinedData <- theCombinedData() %>%
              filter(Faculty != input$selectRemoveFaculty)

            #####################################################
            # For backward compatability, update:               #
            #     rv$available.faculty                          #
            #     rv$loads                                      #
            #####################################################
            modifiedAvailableFaculty <- theRevisedCombinedData %>%
              select("Faculty", "shortSemester", "rank") %>%
              pivot_wider(Faculty, names_from=shortSemester, values_from=rank)
            # if(!turnOffAF){
            #   rv$available.faculty <- rv$combinedData %>%
            #     select("Faculty", "shortSemester", "rank") %>%
            #     pivot_wider(Faculty, names_from=shortSemester, values_from=rank)
            #
            #   rv$loads <- rv$combinedData %>%
            #     mutate(Faculty=shortName) %>%
            #     mutate(semester=longSemester) %>%
            #     select("Faculty", "semester", "load")
            # }
            toReturn$combinedData <- theRevisedCombinedData
            toReturn$afData <- modifiedAvailableFaculty

            removeModal()
          })
          dataModalConfirm <- function(inData){
            ns <- session$ns
            modalDialog(
              h4(paste("You are permanently removing", inData)),
              footer=tagList(
                modalButton("Cancel"),
                actionButton(ns("confirmRemoveFaculty"), "Delete")
              ),
              size="s"
            )
          }
          showModal(dataModalConfirm(isolate(input$selectRemoveFaculty)))
        }, ignoreInit=TRUE, once=TRUE)  #The once=TRUE argument keeps the observer from being triggered multiple times.

        dataModal1 <- function() {
          ns <- session$ns
          beginSemesterNumeric <- theCombinedData() %>%
            filter(longSemester == input$theSemester10BeginNew) %>%
            select("numericSemester") %>%
            unique() %>%
            unlist()
          endSemesterNumeric <- theCombinedData() %>%
            filter(longSemester == input$theSemester10EndNew) %>%
            select("numericSemester") %>%
            unique() %>%
            unlist()

          facultyNames <- theCombinedData() %>%
            filter(numericSemester >= beginSemesterNumeric) %>%
            filter(numericSemester <= endSemesterNumeric) %>%
            filter(!is.na(rank)) %>%
            arrange(Faculty) %>%
            select("Faculty") %>%
            unique() %>%
            unlist()

          modalDialog(
            h4("Permanently remove a faculty member from the database."),
            selectInput(ns("selectRemoveFaculty"), label="Faculty",
                        choices=as.character(facultyNames)),

            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitRemoveFaculty"), "Remove")
            ),
            size="s"
          )
        }
        showModal(dataModal1())
      }, ignoreInit=TRUE, once=FALSE)
      observeEvent(input$facultyAddButton,{
        output$chooseSemesterList11BeginNew <- renderUI({
          #############################################
          # Logic:                                    #
          #   1) The default value at startup will    #
          #      be NULL.                             #
          #   2) If rv$addFacultySemesterBegin        #
          #      is NULL, set it to current semester  #
          #   2) The value may not be after the value #
          #      of the ending semester unless the    #
          #      ending semester value == NULL        #
          #############################################
          ns <- session$ns
          chosenSemester <- input$theSemester10BeginNew
          if (is.null(chosenSemester)) {
            numericCurrentSemester <- CurrentSemester(numeric=TRUE)
            chosenSemester <- inSemesterCodes %>%
              filter(current == as.character(numericCurrentSemester)) %>%
              mutate(longSemester=paste(semester.chr, Year)) %>%
              select(longSemester) %>%
              unlist() %>%
              as.character()
          }
          chosenSemesterIndex <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            filter(longSemester==chosenSemester) %>%
            select(index) %>%
            unlist()

          semestersToDisplay <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            select(longSemester)


          #rv$addFacultySemesterBeginNew <- chosenSemester
          t.out <- selectInput(ns("theSemester11BeginNew"), "First Semester",
                               choices=semestersToDisplay, selected = chosenSemester)

        })
        output$chooseSemesterList11EndNew <- renderUI({
          ##############################################
          # Logic:                                     #
          #   1) The default value at startup will     #
          #      be NULL.                              #
          #   2) If rv$addFacultySemesterEnd           #
          #      is NULL, set it to current semester   #
          #      plus 2                                #
          #   2) The value may not be before the value #
          #      of the beginning semester unless      #
          #      the beginning semester value == NULL  #
          ##############################################
          req(input$theSemester11BeginNew)
          chosenSemester <- NULL
          #chosenSemester <- rv$addFacultySemesterEndNew
          ns <- session$ns
          if (is.null(chosenSemester)) {
            numericCurrentSemester <- CurrentSemester(numeric=TRUE)
            chosenSemester <- inSemesterCodes %>%
              filter(current == as.character(numericCurrentSemester)) %>%
              mutate(longSemester=paste(semester.chr, Year)) %>%
              select(longSemester) %>%
              unlist() %>%
              as.character()
          }
          chosenSemesterIndex <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            filter(longSemester==chosenSemester) %>%
            select(index) %>%
            unlist()

          #cat("!!!!!!!!!!!!!!!!!!!!! addFacultySemesterBeginNew:", rv$addFacultySemesterBeginNew, "\n")
          beginSemesterIndex <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            filter(longSemester==input$theSemester11BeginNew) %>%
            select(index) %>%
            unlist()

          #modify this
          endSemesterIndex <- max(inSemesterCodes$index)

          #add "Undefined" to the list of semestersToDisplay
          semestersToDisplay <- inSemesterCodes %>%
            mutate(longSemester=paste(semester.chr, Year)) %>%
            filter(index >= beginSemesterIndex) %>%
            filter(index <= endSemesterIndex) %>%
            select(longSemester)
          semestersToDisplay <- c("Undefined", semestersToDisplay)

          t.out <- selectInput(ns("theSemester11EndNew"), "End Semester",
                               choices=semestersToDisplay, selected = "Undefined")
        })
        observeEvent(input$submitNewFaculty,{
          # create a new record from information in the modal window
          #      faculty name, rank, and dates of employment
          # Add that information into the rv$available.faculty data
          # that are used to create the form.
          #cat(green("submitNewFaculty button was pushed\n"))
          #cat(green(paste0("input$theSemester11BeginNew:", input$theSemester11BeginNew), "\n"))
          #print(input)
          beginSemester <- inSemesterCodes %>%
            filter(longSemester==input$theSemester11BeginNew) %>%
            select("current") %>%
            unlist()
          #cat("beginSemester:", beginSemester, "\n")

          endSemester <- inSemesterCodes %>%
            filter(longSemester==input$theSemester11EndNew) %>%
            select("current") %>%
            unlist()
          if(length(endSemester)==0) {
            endSemester <- max(inSemesterCodes$current)
          }
          #cat("endSemester:", endSemester, "\n")

          includeSemesters <- theCombinedData() %>%
            filter(numericSemester >= beginSemester) %>%
            filter(numericSemester <= endSemester) %>%
            select("longSemester", "shortSemester", "numericSemester", "sem2") %>%
            unique()

          #cat(green(paste0("newFacultyName:", input$newFacultyName,"\n")))
          newData <- data.frame(Faculty=input$newFacultyName,
                                shortName = unlist(fix.names(input$newFacultyName)),
                                shortNameNoSpace = unlist(fixNamesNoSpace(fix.names(input$newFacultyName))),
                                includeSemesters,
                                rank=as.numeric(input$selectInsertFacultyID),
                                load=NA,
                                assigned.load=NA)

          allFaculty <- theCombinedData()$Faculty
          if(is.na(match(input$newFacultyName, allFaculty))){

            theRevisedCombinedData <- rbind(theCombinedData(), newData)
            #####################################################
            # For backward compatability, update:               #
            #     rv$available.faculty                          #
            #     rv$loads                                      #
            #####################################################

            modifiedAvailableFaculty <- theRevisedCombinedData %>%
              select("Faculty", "shortSemester", "rank") %>%
              pivot_wider(Faculty, names_from=shortSemester, values_from=rank)

            #cat("turnOffAF:", turnOffAF, "\n")
            # if(!turnOffAF){
            #   rv$available.faculty <- rv$combinedData %>%
            #     select("Faculty", "shortSemester", "rank") %>%
            #     pivot_wider(Faculty, names_from=shortSemester, values_from=rank)
            #
            #   rv$loads <- rv$combinedData %>%
            #     mutate(Faculty=shortName) %>%
            #     mutate(semester=longSemester) %>%
            #     select("Faculty", "semester", "load")
            # }
          } else {
            cat("Duplicate Faculty Name.  Not Added!\n")
          }

          toReturn$combinedData <- theRevisedCombinedData
          toReturn$afData <- modifiedAvailableFaculty

          removeModal()
        }, ignoreInit=TRUE, once=TRUE)  #The once=TRUE argument keeps the observer from being triggered multiple times.
        output$addFacultySemesters <- renderUI({
          ns <- session$ns
          #cat(green("In output$addFacultySemesters\n"))
          fluidRow(
            column(6, uiOutput(ns("chooseSemesterList11BeginNew"))),
            column(6, uiOutput(ns("chooseSemesterList11EndNew")))
          )
        })

        dataModal2 <- function() {
          ns <- session$ns
          modalDialog(
            textInput(ns("newFacultyName"), "Faculty Name",
                      placeholder = "Mouse, Minnie"
            ),
            selectInput(ns("selectInsertFacultyID"), label="Rank",
                        choices=c("Asst Prof"=1,
                                  "Assoc Prof"=2,
                                  "Professor"=3,
                                  "VAP"=4,
                                  "Asst Res Sci"=5,
                                  "Asst Res Prof"=6,
                                  "Inst Asst Prof"=7,
                                  "Inst Assc Prof"=8,
                                  "Inst Prof"=9,
                                  "Asst Prof Pract"=10,
                                  "Assoc Prof Pract"=11,
                                  "Prof Practice"=12,
                                  "Lecturer"=13,
                                  "GAL"=14,
                                  "Not Employed"=NA)),
            uiOutput(ns("addFacultySemesters")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitNewFaculty"), "Submit")
            ),
            size="s"
          )
        }

        showModal(dataModal2())

      }, ignoreInit=TRUE, once=FALSE)


      observeEvent(input$lastClickAvailableFacultyName1, {
        observeEvent(input$submitNewName,{
          createShortFacultyName <- function(newLongName){
            #convert newLongName to a short name
            newShortName <- gsub(",.*$", "", newLongName)
            newShortName
          }

          createShortFacultyName2People <- function(newLongName){
            #convert newLongName to a short name with initial b/c
            #there are two people with the same last name
            newShortName <- gsub(",.*$", "", newLongName)
            firstInitial <- substr(str_trim(sub(".*,", "", newLongName)),1,1)
            newShortName <- paste0(firstInitial, ". ", newShortName)
            newShortName
          }

          createShortFacultyNameNoSpace <- function(newLongName){
            #convert newLongName to a short name
            newShortName <- gsub(",.*$", "", newLongName)
            firstInitial <- substr(str_trim(sub(".*,", "", newLongName)),1,1)
            newShortName <- paste0(firstInitial, newShortName)
            newShortName
          }

          updateRank <- function(data, t.fac, inSemester, inIndex, newRank){
            cat(red("Entering updateRank.  \n"))
            cat(blue("t.fac:", t.fac, "\n"))
            cat(blue("inSemester:", inSemester, "\n"))
            cat(blue("index:", inIndex, "\n"))
            cat(blue("newRank:", newRank, "\n"))
            #browser()
            t.sems <- semester.codes %>%
              mutate(sem4.code=paste0("20", current.code))
            #assign("t.data2", data, pos=1)
            t.junk <- data %>%
              pivot_longer(-Faculty, names_to="sem4", values_to="rank1") %>%
              left_join(t.sems, by=c("sem4"="sem4.code")) %>%
              select("Faculty", "sem4", "rank1", "index") #%>%
            # mutate(rank1=case_when(Faculty==t.fac & index>=inIndex & !is.na(rank1)  ~ newRank,
            #                        TRUE ~ rank1)) %>%
            # assign("t.junk2", t.junk, pos=1)
            cat("!!!!!!!!!!!!!!!!Before the mutate\n")
            t.junk <- t.junk %>%
              mutate(rank1=case_when(((Faculty==t.fac) & (index>=inIndex)) ~ as.character(newRank),
                                     TRUE ~ rank1)) %>%

              select("Faculty", "sem4", "rank1") %>%
              pivot_wider(id_cols=Faculty, names_from=sem4, values_from=rank1)
            # assign("t.junk4", t.junk, pos=1)
            #probably need to rebuild instructors here.
            #cat("t.fac:", t.fac, "inSemester:", inSemester, "\n")
            recordExists <- rv$loads %>%
              filter(Faculty==fix.names(t.fac)) %>%
              filter(semester == inSemester)

            cat("recordExists:", nrow(recordExists), "\n")
            if(nrow(recordExists)==0){
              #add a record to the rv$loads for this faculty member and this semester
              cat("********************Adding a record for:", t.fac, "\n")
              t.testing <- rv$loads %>%
                add_row(Faculty=fix.names(t.fac), load=NA, semester=inSemester)
            }
            #assign("t.testing", t.testing, pos=1)

            cat(red("Leaving updateRank.\n"))
            t.junk
          }

          updatedName <- input$fName
          theExistingName <- toReturn$oldFacultyName
          if(updatedName==""){

            updatedName <- theExistingName
          }

          if(theExistingName != updatedName){
            revisedCombinedData <- theCombinedData() %>%
              mutate(Faculty=case_when(
                Faculty == theExistingName ~ updatedName,
                TRUE ~ Faculty)) %>%
              mutate(shortName=unlist(fix.names(Faculty))) %>%
              mutate(shortNameNoSpace=unlist(fixNamesNoSpace(shortName)))
          } else {
            revisedCombinedData <- theCombinedData()
          }

          new.rank <- c(NULL)

          for(i in 1:length(displayedSemesters)){
            inSemNoSpace <- inSemesterCodes %>%
              filter(longSemester==displayedSemesters[i]) %>%
              mutate(semNoSpace=paste0(semester.chr, Year)) %>%
              select("semNoSpace") %>%
              unlist() %>%
              as.vector()
cat(green("Creating new.rank\n"))
            new.rank <- c(new.rank, isolate(input[[paste0("facultyRankSelect_", unlist(fixNamesNoSpace(fix.names(theExistingName))), "_", inSemNoSpace)]]))

          }  #End of for loop

          #cat(yellow("before smallData\n"))
          #convert displayedSemesters to shortSemester format
          displayedSemestersShort <- inSemesterCodes %>%
            filter(longSemester %in% displayedSemesters) %>%
            select("sem4") %>%
            unlist() %>%
            as.vector()
#browser()

          smallData <- data.frame(Faculty=updatedName, shortSemester=displayedSemestersShort, rank=as.integer(new.rank),  stringsAsFactors = FALSE)

          tTemp <- smallData %>%
            anti_join(revisedCombinedData, by=c("Faculty", "shortSemester", "rank")) %>%
            select("Faculty", "shortSemester", "new.rank"="rank") %>%
            left_join(inSemesterCodes, by=c("shortSemester"="semester4")) %>%
            select("Faculty", "shortSemester", "new.rank", "numericSemester"="current", "sem2"="current.code", "longSemester")

          duplicateLastNames <- c("Guneralp", "Bednarz")
#browser()
          revisedCombinedData <- revisedCombinedData %>%
            #mutate(numericSemester=as.character(numericSemester)) %>%
            full_join(tTemp, by=c("Faculty", "shortSemester", "numericSemester", "sem2", "longSemester")) %>%
            mutate(numericSemester=as.numeric(numericSemester)) %>%
            mutate(shortName=createShortFacultyName(Faculty)) %>%
            mutate(shortNameNoSpace=str_trim(shortName)) %>%
            mutate(shortNameNoSpace=case_when((shortName %in% duplicateLastNames) ~ createShortFacultyNameNoSpace(Faculty),
                                              TRUE ~ shortName)) %>%
            mutate(shortName=case_when((shortName %in% duplicateLastNames) ~ createShortFacultyName2People(Faculty),
                                       TRUE ~ shortName)) %>%
            mutate(rank=as.integer(rank)) %>%
            mutate(rank=case_when(
              !is.na(new.rank) ~ new.rank,
              TRUE ~ rank
            )) %>%
            naniar::replace_with_na(replace=list(rank=-9999)) %>%
            select("recnum", "Faculty", "shortName", "shortNameNoSpace", "longSemester", "shortSemester", "numericSemester", "sem2", "rank", "load", "assigned.load")

          revisedCombinedData <- revisedCombinedData %>%
            mutate(load=case_when(
              is.na(rank) ~ NA_real_,
              TRUE ~ as.numeric(load)
            ))


          revisedMasterCourses <- theMasterCourses() %>%
            mutate(Faculty=case_when(instructor == createShortFacultyName(theExistingName) ~ createShortFacultyName(updatedName),
                                     TRUE ~ instructor))
          modifiedAvailableFaculty <- revisedCombinedData %>%
            select("Faculty", "shortSemester", "rank") %>%
            pivot_wider(Faculty, names_from=shortSemester, values_from=rank)

          modifiedLeaveData <- theLeaveData() %>%
            mutate(faculty=case_when(faculty == createShortFacultyName(theExistingName) ~ createShortFacultyName(updatedName),
                                     TRUE ~ as.character(faculty)))

          if(allowUpdate){
            saveRDS(rv$combinedData, paste0(rds.path, "combinedData.rds"))
          }

          toReturn$mcData <- revisedMasterCourses
          toReturn$combinedData <- revisedCombinedData
          toReturn$leaveData <- modifiedLeaveData
          toReturn$afData <- modifiedAvailableFaculty
          cat("watch point\n")
          #browser()
          removeModal()
        }, ignoreInit=TRUE, once=FALSE)  #The once=TRUE argument keeps the observer from being triggered multiple times.

        dataModal <- function(inFacultyMemberName) {
          ns <- session$ns
          rankDisplayFunction <- function(inSem, inFacultyMemberName){
            #cat(yellow("[rankDisplayFunction] inFacultyMemberName:", inFacultyMemberName, "\n"))
            #cat(yellow("[rankDisplayFunction] inSem:", inSem, "\n"))
            #browser()
            facultyRankSelect <- theCombinedData() %>%
              filter(Faculty==inFacultyMemberName) %>%
              filter(longSemester==inSem) %>%
              mutate(rank=case_when(
                is.na(rank) ~ -9999,
                TRUE ~ as.numeric(rank)
              )) %>%
              pull("rank")


            #cat(yellow("[rankDisplayFunction] facultyRankSelect:", facultyRankSelect, "\n"))

            inSemNoSpace <- inSemesterCodes %>%
              filter(longSemester==inSem) %>%
              mutate(semNoSpace=paste0(semester.chr, Year)) %>%
              select("semNoSpace") %>%
              unlist() %>%
              as.vector()

            selectInputID <- paste0("facultyRankSelect_", gsub("[^[:alnum:]]","", unlist(fix.names(inFacultyMemberName))),"_",inSemNoSpace)

            fluidRow(
              column(4, h5(inSem)),
              column(8, selectInput(ns(selectInputID), label=NULL,
                                    choices=c("Asst Prof"=1,
                                              "Assoc Prof"=2,
                                              "Professor"=3,
                                              "VAP"=4,
                                              "Asst Res Sci"=5,
                                              "Res Asst Prof"=6,
                                              "Inst Asst Prof"=7,
                                              "Inst Assc Prof"=8,
                                              "Inst Prof"=9,
                                              "Asst Prof Pract"=10,
                                              "Assoc Prof Pract"=11,
                                              "Prof Practice"=12,
                                              "Lecturer"=13,
                                              "GAL"=14,
                                              "Not Employed"=-9999
                                    ),
                                    selected=facultyRankSelect))

            )
          }

          facultyMemberName <- theCombinedData() %>%
            filter(shortNameNoSpace==inFacultyMemberName) %>%
            select("Faculty") %>%
            unlist() %>%
            as.character() %>%
            unique()
          cat("facultyMemberName:", facultyMemberName, "\n")
          toReturn$oldFacultyName <- facultyMemberName
          modalDialog(
            textInput(ns("fName"), "Edit Name",
                      placeholder = facultyMemberName
            ),
            tagList(
              lapply(displayedSemesters, FUN=rankDisplayFunction, inFacultyMemberName=facultyMemberName)
            ),
            actionButton(ns("leaveButton"), "Add Leave"),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("submitNewName"), "Submit")
            ),
            size="s"
          )
        }

        t.elements <- unlist(strsplit(input$lastClickAvailableFacultyNameId, "_"))
        inFaculty <- t.elements[2]

        beginSemesterIndex <- inSemesterCodes %>%
          filter(longSemester == input$theSemester10BeginNew) %>%
          pull("index")


        endSemesterIndex <- inSemesterCodes %>%
          filter(longSemester == input$theSemester10EndNew) %>%
          pull("index")

        displayedSemesters <- inSemesterCodes %>%
          filter(index>= beginSemesterIndex) %>%
          filter(index <= endSemesterIndex) %>%
          pull("longSemester")

#browser()
        showModal(dataModal(inFaculty))
      })
      observeEvent(input$lastClickAvailableFaculty1 ,{
        t.elements <- unlist(strsplit(input$lastClickAvailableFacultyId, "_"))
        inFaculty <- t.elements[2]
        inSemester <- t.elements[3]
        inSemesterLong <- inSemesterCodes %>%
          filter(current.code==inSemester) %>%
          select("longSemester") %>%
          unlist() %>%
          as.character()
        inLongFacultyName <- theCombinedData() %>%
          filter(shortNameNoSpace == inFaculty) %>%
          select("Faculty") %>%
          unlist() %>%
          unique() %>%
          as.character()
        inShortFacultyNameWithInitial <- theCombinedData() %>%
          filter(shortNameNoSpace == inFaculty) %>%
          select("shortName") %>%
          unlist() %>%
          unique() %>%
          as.character()

        existingLoad <- theCombinedData() %>%
          filter(shortName==inShortFacultyNameWithInitial) %>%
          filter(longSemester==inSemesterLong) %>%
          select("load") %>%
          pull(load) %>%
          as.numeric()

        incrementLoadValue <- function(load, max.load=6){
          if(is.na(load)){
            newLoad <- 0
          } else if ((load+1) > max.load) {
            newLoad <- -9999
          } else {
            newLoad <- load+1
          }
          newLoad
        }
#browser()
# the problem is with TRUE ~ load

        theRevisedCombinedData <- theCombinedData() %>%
          mutate(load = as.numeric(load)) %>%
          mutate(load = case_when((Faculty==inLongFacultyName) &
                                    (longSemester==inSemesterLong) ~ incrementLoadValue(existingLoad),
                                  TRUE ~ load))
        theRevisedCombinedData <- theRevisedCombinedData %>%
          replace_with_na(replace = list(load = c(-9999)))

        toReturn$combinedData <- theRevisedCombinedData
        if(allowUpdate){
          saveRDS(rv$combinedData, file=".//Data//combinedData.rds")
        }
      })
      observeEvent(input$leaveButton, {
        observeEvent(input$confirmLeave, {
          cat("Adding a Leave to facultyLeaves\n")
          #cat(unlist(fix.names(input$facultyNameLeave)), "\n")
          #cat(green("faculty:", faculty, "\n"))
          #convert faculty to shortName format
          #print(names(theCombinedData()))
          shortName <- theCombinedData() %>%
            filter(Faculty==toReturn$oldFacultyName) %>%
            select("shortName") %>%
            unique() %>%
            unlist() %>%
            as.vector()
          cat(green("shortName:", shortName, "\n"))
          #print(toReturn$leaveData)
          cat(green("after print(theLeaveData)\n"))
          print(names(theLeaveData()))
          newRecNum <- max(theLeaveData()$recnum)+1
          # find the UIN
          facultyUIN <- facultyUINs %>%
            filter(displayName==shortName) %>%
            pull(UIN)
#browser()
          theRevisedLeaveData <- theLeaveData() %>%
            # add_row(faculty=unlist(fix.names(input$facultyNameLeave)),
            #         semester=convertSemester2(input$facultyLeaveSemester),
            #         leaveType=input$facultyLeaveType) %>%
            add_row(recnum=newRecNum,
                    faculty=unlist(shortName),
                    UIN=facultyUIN,
                    semester=convertSemester2(input$facultyLeaveSemester),
                    leaveType=input$facultyLeaveType) %>%
            unique() %>%
            filter(!is.na(faculty))
          print(theRevisedLeaveData)
          # rv$facultyLeave <- rv$facultyLeave %>%
          #   add_row(faculty=unlist(fix.names(input$facultyNameLeave)),
          #           semester=convertSemester2(input$facultyLeaveSemester),
          #           leaveType=input$facultyLeaveType) %>%
          #   unique()


          if(allowUpdate){
            saveRDS(rv$facultyLeave, ".//Data//facultyLeave.rds")
          }
          cat(blue("adding theRevisedLeaveData to toReturn$leaveData\n"))
          toReturn$leaveData <- theRevisedLeaveData
          removeModal()
        }, once=TRUE)
        dataModalLeave <- function(semesters, faculty){
          ns <- session$ns
          #cat(yellow("faculty:", faculty, "\n"))
          #browser()
          modalDialog(
            h4(paste("Add a leave type")),
            selectInput(ns("facultyLeaveSemester"), "Semester", choices=semesters),
            #selectInput(ns("facultyNameLeave"), "Faculty", choices=faculty),
            selectInput(ns("facultyLeaveType"), "Leave Type", choices=c("Family Leave", "Faculty Development Leave",
                                                                        "Negotiated Leave", "Administrative Teaching Reduction",
                                                                        "Mid-tenure Leave", "Unpayed Leave",
                                                                        "Course Buyout"), selected="Family Leave"),
            footer=tagList(
              modalButton("Cancel"),
              actionButton(ns("confirmLeave"), "Add")
            ),
            size="s"
          )
        }

#browser()
        beginSemesterNumeric <- theCombinedData() %>%
          filter(longSemester == input$theSemester10BeginNew) %>%
          pull(numericSemester)
          # select("numericSemester") %>%
          # unique() %>%
          # unlist()
        endSemesterNumeric <- theCombinedData() %>%
          filter(longSemester == input$theSemester10EndNew) %>%
          pull(numericSemester)
          # select("numericSemester") %>%
          # unique() %>%
          # unlist()

        semesters <- theCombinedData() %>%
          filter(numericSemester >= beginSemesterNumeric) %>%
          filter(numericSemester <= endSemesterNumeric) %>%
          pull(shortSemester) %>%
          unique()
          # select("shortSemester") %>%
          # unique() %>%
          # unlist() %>%
          # as.vector()
        #browser()  #check if semesters contain data

        # t.elements <- unlist(strsplit(input$lastClickAvailableFacultyNameId, "_"))
        # inFaculty <- t.elements[2]

        showModal(dataModalLeave(semesters, toReturn$oldFacultyName))
      }, ignoreInit=TRUE)

      #############################
      # Utility Functions         #
      #############################
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
            t.letter <- "31"
          } else if (current.date < t.b) {
            t.letter <- "21"
          } else t.letter <- "11"
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
      fixNamesNoSpace <- function(inData){
        outData <- gsub(". ", "", inData, fixed=TRUE)
        outData <- gsub("'", "", outData, fixed=TRUE)  #New addition to solve problem with adding employment
        #outData <- gsub("’", "", outData, fixed=TRUE)  #New addition to solve problem with adding employment
        outData <- gsub(" ", "", outData, fixed=TRUE)
        outData
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
      convertSemester2 <- function(inputSemester, semesterTable=inSemesterCodes){
        outputSemester <- semesterTable %>%
          mutate(longSemester=paste(semester.chr, Year)) %>%
          mutate(sem4=paste0("20", current.code)) %>%
          filter(sem4==inputSemester) %>%
          select(longSemester) %>%
          as.character() %>%
          unlist()
        outputSemester
      }
      #browser()
      return(toReturn)
    }
  )
}
