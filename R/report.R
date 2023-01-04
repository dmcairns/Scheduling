#' reportModuleUI
#'
#' @param id The id
#'
#' @return
#' @export
#'
#' @examples
reportModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
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
                                    #column(12, uiOutput(ns("legacyReport1")))
                                    column(12, uiOutput(ns("revisedReport2022")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' reportModuleServer
#'
#' @param id The id
#' @param input The input
#' @param output The output
#' @param session The session
#' @param inSemester semester passed into function
#' @param theMasterCourses the master list of courses
#' @param theCombinedData the combined data
#' @param theLeaveData leave data
#' @param inSemesterCodes all semester codes
#' @param synchronize whether or not the data are synchronized
#' @param chosenSemester which semester is chosen
#'
#' @return
#' @export
#'
#' @examples
reportModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                               theCombinedData, theLeaveData, inSemesterCodes, synchronize=NULL,
                               chosenSemester=NULL){
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
      displayedInstructors <- 0
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
      observeEvent(input$theSemester1,{
        if(is.null(synchronize)){
          toReturn$viewSemester <- input$theSemester1
        } else if (!synchronize()){
          toReturn$viewSemester <- input$theSemester1
        }

      })

      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Teaching Assignments for ", input$theSemester1)
        theTitle
      })
      output$legacyReport1 <- renderUI({
        cat(yellow("Entered legacyReport1\n"))
        req(inSemester())     #May not be necessary
        req(theMasterCourses())
        req(theCombinedData())
        cat(yellow("Past req statements\n"))
        ns <- session$ns
        renderHeader <- TRUE
        #displayedInstructors <- 0


        find.courses <- function(data, data.summary, i){
          #pull out the courses that are taught by a specific instructor
          instructor.of.interest <- data.summary[i,"instructor"]
          courses.taught <- data %>%
            filter(instructor==unlist(instructor.of.interest)) %>%
            select(courseID, OL) %>%
            arrange(courseID)
          courses.taught
        }
        update.CSS.local <- function(input.name, index, class.name){
          addClass(paste0(input.name,index), class.name)
        }
        extract.course <- function(data, index){
          t.out <- data[index,]
          last.number <<- last.number + 1
          #last.number <- last.number + 1
          t.out
        }
        individual.element <- function(in.data, i){

          if(dim(data.frame(in.data))[[1]]==0) t.output <- NULL
          else {
            display.class <- "LegacyReportOfferedCourse"
            if(in.data[[i]]$OL) display.class <- "LegacyReportOfferedCourseOnline"
            t.output <- column(2,div(in.data[[i]]$courseID, class=display.class))
          }
          t.output
        }
        create.textboxes <- function(data, data.summary, i){
          courses.found <- find.courses(data, data.summary, i)
          numCourses <- dim(courses.found)[1]
          t.out <- lapply(1:(numCourses), function(j) {
            extract.course(courses.found, j)
          })

          if(is.na(t.out[[1]][2])) {
            instructor.of.interest <- data.summary[i,"instructor"]
            instructor.of.interest <- as.vector(unlist(instructor.of.interest))

            # if load for instructor in the selected semester is zero,
            # then there needs to be a reason for it.  Otherwise, the lack of
            # courses just means that they haven't been assigned yet.

            t.load <- theCombinedData() %>%
              mutate(shortName=displayName) %>%
              filter(shortName == instructor.of.interest) %>%
              filter(longSemester == input$theSemester1) %>%
              select("Faculty"="shortName", "semester"="longSemester", "load") %>%
              mutate(load = case_when(
                is.na(load) ~ 0,
                TRUE ~ as.numeric(load)
              )) %>%
              unlist()

            if(as.numeric(t.load[3]) == 0) {
              leaveType <- theLeaveData() %>%
                filter(faculty==instructor.of.interest) %>%
                filter(semester==input$theSemester1) %>%
                select(leaveType) %>%
                unlist()
              the.output <- list(
                #output of the type individual element
                column(6,div(leaveType))
              )
            } else the.output <- NULL
          }
          else {
            if(numCourses==1){
              the.output <- list(individual.element(t.out,1))
            } else if(numCourses==2){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2)
              )
            } else if(numCourses==3){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3)
              )
            } else if(numCourses ==4){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4)
              )
            } else if(numCourses ==5){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5)
              )
            } else if(numCourses ==6){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6)
              )
            } else if(numCourses ==7){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6),
                individual.element(t.out,7)
              )
            } else if(numCourses ==8){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6),
                individual.element(t.out,7),
                individual.element(t.out,8)
              )

            } else {
              cat("******************************* More than two **************\n")
              for(k in 1:numCourses){
                if(k==1) the.output <- tagList(individual.element(t.out, k))
                else the.output <- c(the.output, individual.element(t.out, k))
              }
            }
          }

          the.output
        }

        calc.courses.assigned.including.stacked.New <- function(data.faculty, in.semester){

          output <- theCombinedData() %>%
            filter(shortName==data.faculty) %>%
            filter(longSemester==in.semester) %>%
            select("assigned.load") %>%
            mutate(assigned.load=case_when(
              is.na(assigned.load) ~ 0,
              TRUE ~ assigned.load
            )) %>%
            unlist()
        }
        calc.annual.courses.assigned.including.stacked.New <- function(data.faculty, in.semester){

          term <- gsub(" .*$", "", in.semester)
          year <- as.numeric(unlist(str_extract_all(in.semester, "[0-9]+")))
          if(term=="Fall"){
            other.semester <- paste("Spring", year+1)
            summer.term <- paste("Summer", year+1)
          } else if(term=="Spring"){
            other.semester <- paste("Fall", year-1)
            summer.term <- paste("Summer", year)
          } else {
            fall.semester <- paste("Fall", year-1)
            spring.semester <- paste("Spring", year)
          }
          if(term != "Summer"){
            sem1 <- calc.courses.assigned.including.stacked.New(data.faculty, in.semester)
            sem2 <- calc.courses.assigned.including.stacked.New(data.faculty, other.semester)
            summer <- calc.courses.assigned.including.stacked.New(data.faculty, summer.term)
          } else {
            sem1 <- calc.courses.assigned.including.stacked.New(data.faculty, fall.semester)
            sem2 <- calc.courses.assigned.including.stacked.New(data.faculty, spring.semester)
            summer <- calc.courses.assigned.including.stacked.New(data.faculty, in.semester)
          }

          #sum(sem1,sem2)
          c(sem1, sem2, summer)
        }
        #combine t.summary.info and available faculty to get zero loads.

        fix.names <- function(data){
          check.duplicate.names <- function(in.data){
            t.substr <- gsub(",.*$", "", in.data)
            t.first <- gsub('.*,\\s*','', in.data)
            t.first <- substr(t.first, 1,1)

            if((t.substr=="Bednarz")|(t.substr=="Guneralp")|(t.substr=="Zhang"))
              t.out <- paste0(t.first, ". ", t.substr)
            else
              t.out <- t.substr
            t.out
          }
          t.out <- lapply(data, check.duplicate.names)
          t.out
        }

        req(input$theSemester1)
        t.sem <- input$theSemester1
        semester.code.new <- inSemesterCodes %>%
          mutate(sem.name=paste(semester.chr, Year)) %>%
          filter(sem.name==t.sem) %>%
          mutate(sem5=paste0("20", current.code)) %>%
          select(sem5) %>%
          unlist()

        semester.data <- theMasterCourses() %>%
          filter(semester==input$theSemester1)

        cat(blue("displayedInstructors:", displayedInstructors, "\n"))

        if(dim(semester.data)[1] == 0){
          t.out <- h4("No courses are scheduled to be taught this term.  Add courses using the Course Selection tab.", style="color: red")
        } else{
          summary.info <- semester.data %>%
            group_by(instructor) %>%
            summarise(num.assignments=n(), .groups="drop")

          summary.info <- theCombinedData() %>%
            mutate(shortName=displayName) %>%
            filter(longSemester == input$theSemester1) %>%
            filter(!is.na(rank)) %>%
            select("instructor"="shortName", "rank", "load", "assigned.load") %>%
            arrange(instructor) %>%
            left_join(summary.info, by=c("instructor")) %>%
            mutate(num.assignments=case_when(
              is.na(num.assignments) ~ 0.0,
              TRUE ~ as.double(num.assignments)
            )) %>%
            mutate(load=case_when(
              is.na(load) ~ 0.0,
              TRUE ~ as.double(load)
            )) %>%
            mutate(assigned.load=case_when(
              is.na(assigned.load) ~ 0.0,
              TRUE ~ as.double(assigned.load)
            ))

          t.summary.info <- summary.info


          assign("last.number", 0, pos=1)
          IDs <- seq_len(nrow(semester.data))
          numInstructors <- dim(summary.info)[1]
          cat(yellow("numInstructors:", numInstructors, "\n"))

          t.out <- lapply(1:(numInstructors), function(i) {
            # determine load level (OK, overload, underload), assign to css.courseLoadStatus
            #browser()
            contracted.load <- summary.info %>%
              filter(instructor==t.summary.info$instructor[i]) %>%
              select(load) %>%
              unlist()
            modified.assigned.load <- summary.info %>%
              filter(instructor==summary.info$instructor[i]) %>%
              select(assigned.load) %>%
              unlist()
            annual.load <- calc.annual.courses.assigned.including.stacked.New(t.summary.info$instructor[i], input$theSemester1)

            if(is.na(contracted.load)) {
              t.out.partial <- NULL
            } else {
              displayedInstructors <<- displayedInstructors + 1
              if(unlist(contracted.load)==modified.assigned.load){
                css.courseLoadStatus <- "LegacyReportFacultyOK"
              } else {
                if(unlist(contracted.load) > modified.assigned.load){
                  css.courseLoadStatus <- "LegacyReportFacultyUnderload"
                } else css.courseLoadStatus <- "LegacyReportFacultyOverload"
              }
              if (displayedInstructors %% 2 == 0){
                cssClass <- "oddRow2"
              } else {
                cssClass <- "evenRow2"
              }
              fallOrSpring <- inSemesterCodes %>%
                filter(longSemester==input$theSemester1) %>%
                select("semester.chr") %>%
                unlist() %>%
                as.vector()
              if(fallOrSpring=="Fall"){
                AL1 <- annual.load[1]
                AL2 <- annual.load[2]
                SL <- annual.load[3]
              } else if (fallOrSpring == "Spring") {
                AL1 <- annual.load[2]
                AL2 <- annual.load[1]
                SL <- annual.load[3]
              } else {
                AL1 <- annual.load[2]
                AL2 <- annual.load[1]
                SL <- annual.load[3]
              }
              if(renderHeader==TRUE){

                t.out.partial1 <- list(
                  div(id="rowLegacyReportHeader",
                      fluidRow(
                        div(column(2, div("Faculty"))),
                        div(column(2, div("Courses Assigned (F/Sp/Su)"))),
                        div(column(2, div("Courses")))
                      ))
                )
                renderHeader <<- FALSE


              } else t.out.partial1 <- NULL


              t.out.partial2 <- list(
                div(id=paste0("rowLegacyReport", i),
                    fluidRow(

                      div(column(2, div(summary.info$instructor[i], class=css.courseLoadStatus))),
                      div(column(2, div(paste(AL1, "/", AL2, "/", SL), class=css.courseLoadStatus))),
                      div(column(8, create.textboxes(semester.data, summary.info, i), inline=TRUE))
                    ), class=cssClass
                )
              )
              t.out.partial <- list(t.out.partial1, t.out.partial2)

            }
            t.out.partial
          })#end of lapply that generates the UI

          t.out.check <- t.out
        }

        cat(red("displayedInstructors:", displayedInstructors, "\n"))
        if(displayedInstructors == 0){
          t.out <- h4(paste0("No instructors have assigned courses for ", input$theSemester1, "."), style="color: red")
        }
        t.out
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
        req(input$theSemester4)
        unassigned.courses <- rv$master.courses %>%
          filter(semester==input$theSemester4) %>%
          filter(instructor=="Unassigned") %>%
          arrange(courseID, section)

        print(unassigned.courses)
        w.courses <- rv$master.courses %>%
          filter(semester==input$theSemester4) %>%
          filter(W==TRUE) %>%
          arrange(courseID, section)

        tagList(
          br(),
          h3("Unassigned Courses"),
          DT::dataTableOutput(ns("UnassignedTable")),

          #Display a list of Regional Courses Staffed
          #Display a list of Writing Courses Staffed
          br(),
          h3("Writing Courses Staffed"),
          DT::dataTableOutput(ns("WTable"))

          #Display a list of classes taught by GALs
          #    Need a list of full-time salaried faculty
        )

      })

      output$revisedReport2022 <- renderUI({

        ########################################
        # Required Data                        #
        ########################################
        req(inSemester())
        req(theMasterCourses())
        req(theCombinedData())
        req(input$theSemester1)

        ########################################
        # Global values initialization         #
        ########################################

        ns <- session$ns
        renderHeader <- TRUE

        ########################################
        # Report specific Functions            #
        ########################################

        find.courses <- function(data, data.summary, i){
          #pull out the courses that are taught by a specific instructor
          #instructor.of.interest <- data.summary[i,"instructor"]
          instructor.of.interest <- data.summary[i,"Faculty"]
          courses.taught <- data %>%
            filter(instructor==unlist(instructor.of.interest)) %>%
            select(courseID, OL) %>%
            arrange(courseID)
          courses.taught
        }
        update.CSS.local <- function(input.name, index, class.name){
          addClass(paste0(input.name,index), class.name)
        }
        extract.course <- function(data, index){
          t.out <- data[index,]
          last.number <<- last.number + 1
          #last.number <- last.number + 1
          t.out
        }
        individual.element <- function(in.data, i){

          if(dim(data.frame(in.data))[[1]]==0) t.output <- NULL
          else {
            display.class <- "LegacyReportOfferedCourse"
            if(in.data[[i]]$OL) display.class <- "LegacyReportOfferedCourseOnline"
            t.output <- column(2,div(in.data[[i]]$courseID, class=display.class))
          }
          #cat("individual.element:", in.data[[i]]$courseID, "\n")
          t.output
        }
        create.textboxes <- function(data, data.summary, i){
          #if(i==3) browser()
          courses.found <- find.courses(data, data.summary, i)
          numCourses <- dim(courses.found)[1]
          t.out <- lapply(1:(numCourses), function(j) {
            extract.course(courses.found, j)
          })

          if(is.na(t.out[[1]][2])) {
            #cat("***********************In is.na(t.out):\n")
            instructor.of.interest <- data.summary[i,"Faculty"]
            instructor.of.interest <- as.vector(unlist(instructor.of.interest))

            # if load for instructor in the selected semester is zero,
            # then there needs to be a reason for it.  Otherwise, the lack of
            # courses just means that they haven't been assigned yet.

            t.load <- theCombinedData() %>%
              mutate(shortName=displayName) %>%
              filter(shortName == instructor.of.interest) %>%
              #filter(longSemester == inSemester()) %>%
              filter(longSemester == input$theSemester1) %>%
              select("Faculty"="shortName", "semester"="longSemester", "load") %>%
              mutate(load = case_when(
                is.na(load) ~ 0,
                TRUE ~ as.numeric(load)
              )) %>%
              unlist()

            if(as.numeric(t.load[3]) == 0) {
              leaveType <- theLeaveData() %>%
                filter(faculty==instructor.of.interest) %>%
                #filter(semester==inSemester()) %>%
                filter(semester==input$theSemester1) %>%
                select(leaveType) %>%
                unlist()
              the.output <- list(
                #output of the type individual element
                column(6,div(leaveType))
              )
            } else the.output <- NULL
          }
          else {
            if(numCourses==1){
              the.output <- list(individual.element(t.out,1))
            } else if(numCourses==2){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2)
              )
            } else if(numCourses==3){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3)
              )
            } else if(numCourses ==4){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4)
              )
            } else if(numCourses ==5){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5)
              )
            } else if(numCourses ==6){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6)
              )
            } else if(numCourses ==7){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6),
                individual.element(t.out,7)
              )
            } else if(numCourses ==8){
              the.output <- list(
                individual.element(t.out,1),
                individual.element(t.out,2),
                individual.element(t.out,3),
                individual.element(t.out,4),
                individual.element(t.out,5),
                individual.element(t.out,6),
                individual.element(t.out,7),
                individual.element(t.out,8)
              )

            } else {
              cat("******************************* More than two **************\n")
              for(k in 1:numCourses){
                if(k==1) the.output <- tagList(individual.element(t.out, k))
                else the.output <- c(the.output, individual.element(t.out, k))
              }
            }
          }

          the.output
        }

        calc.courses.assigned.including.stacked.New <- function(data.faculty, in.semester){

          output <- theCombinedData() %>%
            filter(shortName==data.faculty) %>%
            filter(longSemester==in.semester) %>%
            select("assigned.load") %>%
            mutate(assigned.load=case_when(
              is.na(assigned.load) ~ 0,
              TRUE ~ assigned.load
            )) %>%
            unlist()
        }
        calc.annual.courses.assigned.including.stacked.New <- function(data.faculty, in.semester){

          term <- gsub(" .*$", "", in.semester)
          year <- as.numeric(unlist(str_extract_all(in.semester, "[0-9]+")))
          if(term=="Fall"){
            other.semester <- paste("Spring", year+1)
            summer.term <- paste("Summer", year+1)
          } else if(term=="Spring"){
            other.semester <- paste("Fall", year-1)
            summer.term <- paste("Summer", year)
          } else {
            fall.semester <- paste("Fall", year-1)
            spring.semester <- paste("Spring", year)
          }
          if(term != "Summer"){
            sem1 <- calc.courses.assigned.including.stacked.New(data.faculty, in.semester)
            sem2 <- calc.courses.assigned.including.stacked.New(data.faculty, other.semester)
            summer <- calc.courses.assigned.including.stacked.New(data.faculty, summer.term)
          } else {
            sem1 <- calc.courses.assigned.including.stacked.New(data.faculty, fall.semester)
            sem2 <- calc.courses.assigned.including.stacked.New(data.faculty, spring.semester)
            summer <- calc.courses.assigned.including.stacked.New(data.faculty, in.semester)
          }

          #sum(sem1,sem2)
          c(sem1, sem2, summer)
        }

        extractCurrentSemesterData <- function(inSemester){
          semester.data <- theMasterCourses() %>%
            filter(semester==input$theSemester1)
          semester.data
        }

        extractAcademicYearData <- function(inSemester){

          ##########################################
          # Academic Year begins in Fall and then  #
          # continues into Spring and Summer.      #
          # AY2023 would include Fall 2022,        #
          # Spring 2023, Summer 2023               #
          ##########################################

          ##########################################
          # Data used from global environment:     #
          #   inSemesterCodes                      #
          #   theMasterCourses()                   #
          #   theCombinedData()                    #
          ##########################################

          # Local semester code data creation (Adding academicYear to data frame)
          localSemesterCodes <- inSemesterCodes %>%
            mutate(academicYear=case_when(semester.chr == "Fall" ~ Year+1,
                                          TRUE ~ Year)
                   ) %>%
            select("semester.display", "academicYear")

          # Determine the Academic Year
          theAcademicYear <- localSemesterCodes %>%
            filter(semester.display==inSemester) %>%
            pull(academicYear)

          # Add the Academic Year by joining masterCourses to localSemesterCodes


          localMasterCoursesSingleAcademicYear <- theMasterCourses() %>%
            left_join(localSemesterCodes, by=c("semester"="semester.display")) %>%
            filter(academicYear == theAcademicYear)

          # The code below is a hack.  load.contribution is sometimes listed as NA.
          # If load.contribution is NA, mutate it to 1 to avoid problems.
          # Fix this elsewhere in the code when load.contribution is originally determined.

          localMasterCoursesSingleAcademicYear <- localMasterCoursesSingleAcademicYear %>%
            mutate(load.contribution=case_when(is.na(load.contribution) ~ 1.0,
                                               TRUE ~ load.contribution))

          localIndividualSemestersAssigned <- localMasterCoursesSingleAcademicYear %>%
            mutate(season=gsub( " .*$", "", semester)) %>%
            select("UIN", "season", "load.contribution") %>%
            group_by(UIN, season) %>%
            mutate(load.contribution=case_when(is.na(load.contribution) ~ 0,
                                               TRUE ~ load.contribution)) %>%
            summarize(load.sum=sum(load.contribution)) %>%
            pivot_wider(id_cols=c("UIN"), names_from=season, values_from=load.sum) %>%
            rename("Fall.assigned"="Fall", "Spring.assigned"="Spring") %>%
            mutate(Fall.assigned = case_when(is.na(Fall.assigned) ~ 0,
                                             TRUE ~ Fall.assigned)) %>%
            mutate(Spring.assigned = case_when(is.na(Spring.assigned) ~ 0,
                                               TRUE ~ Spring.assigned))

          if("Summer" %in% names(localIndividualSemestersAssigned)){
            localIndividualSemestersAssigned <- localIndividualSemestersAssigned %>%
              rename("Summer.assigned"="Summer") %>%
              mutate(Summer.assigned=case_when(is.na(Summer.assigned) ~ 0,
                                               TRUE ~ Summer.assigned))
          } else {
            localIndividualSemestersAssigned <- localIndividualSemestersAssigned %>%
              mutate(Summer.assigned=0)
          }

          # and filter out the summers.
          localMasterCoursesSingleAcademicYear <- localMasterCoursesSingleAcademicYear %>%
            filter(semester != paste("Summer", academicYear))

          # Add the Academic Year by joining theCombinedData to localSemesterCodes

          localCombinedDataAcademicYear <- theCombinedData() %>%
            left_join(localSemesterCodes, by=c("longSemester"="semester.display")) %>%
            filter(academicYear == theAcademicYear)


          theRanks <- localCombinedDataAcademicYear %>%
            filter(longSemester==inSemester) %>%
            select("UIN", "rank")

          localIndividualSemestersContracted <- localCombinedDataAcademicYear %>%
            mutate(season=gsub( " .*$", "", longSemester)) %>%
            select("UIN", "season", "load") %>%
            mutate(load=case_when(is.na(load) ~ as.integer(0), TRUE ~ as.integer(load))) %>%
            pivot_wider(id_cols=c("UIN"), names_from=season, values_from=load) %>%
            rename("Fall.contracted"="Fall", "Spring.contracted"="Spring", "Summer.contracted"="Summer")


          # and filter out the summers.
          localCombinedDataAcademicYear <- localCombinedDataAcademicYear %>%
            filter(longSemester != paste("Summer", academicYear))

          # Calculate Annual Load (not including summer) by faculty Member

          annualAssignedLoads <- localMasterCoursesSingleAcademicYear %>%
            group_by(UIN, Faculty) %>%
            summarize(annualAssignedLoad=sum(load.contribution, na.rm=TRUE), .groups="drop")

          annualContractedLoads <- localCombinedDataAcademicYear %>%
            group_by(UIN) %>%
            summarize(annualContractedLoad=sum(load, na.rm=TRUE), .groups="drop")


          annualLoads <- annualAssignedLoads %>%
            left_join(annualContractedLoads, by=c("UIN")) %>%
            left_join(localIndividualSemestersContracted, by=c("UIN")) %>%
            left_join(localIndividualSemestersAssigned, by=c("UIN")) %>%
            left_join(theRanks, by=c("UIN")) %>%
            filter(!is.na(rank)) %>%
            arrange(Faculty) %>%
            select(-"rank")


          annualLoads

        }

        generateHTML4ReportOrig <- function(i) {
          contracted.load <- summary.info %>%
            filter(instructor==t.summary.info$instructor[i]) %>%
            select(load) %>%
            unlist()
          modified.assigned.load <- summary.info %>%
            filter(instructor==summary.info$instructor[i]) %>%
            select(assigned.load) %>%
            unlist()
          #browser()

          annual.load <- calc.annual.courses.assigned.including.stacked.New(t.summary.info$instructor[i], input$theSemester1)

          if(is.na(contracted.load)) {
            t.out.partial <- NULL
          } else {
            displayedInstructors <<- displayedInstructors + 1
            if(unlist(contracted.load)==modified.assigned.load){
              css.courseLoadStatus <- "LegacyReportFacultyOK"
            } else {
              if(unlist(contracted.load) > modified.assigned.load){
                css.courseLoadStatus <- "LegacyReportFacultyUnderload"
              } else css.courseLoadStatus <- "LegacyReportFacultyOverload"
            }
            if (displayedInstructors %% 2 == 0){
              cssClass <- "oddRow2"
            } else {
              cssClass <- "evenRow2"
            }
            fallOrSpring <- inSemesterCodes %>%
              filter(longSemester==input$theSemester1) %>%
              select("semester.chr") %>%
              unlist() %>%
              as.vector()
            if(fallOrSpring=="Fall"){
              AL1 <- annual.load[1]
              AL2 <- annual.load[2]
              SL <- annual.load[3]
            } else if (fallOrSpring == "Spring") {
              AL1 <- annual.load[2]
              AL2 <- annual.load[1]
              SL <- annual.load[3]
            } else {
              AL1 <- annual.load[2]
              AL2 <- annual.load[1]
              SL <- annual.load[3]
            }
            if(renderHeader==TRUE){

              t.out.partial1 <- list(
                div(id="rowLegacyReportHeader",
                    fluidRow(
                      div(column(2, div("Faculty"))),
                      div(column(2, div("Courses Assigned (F/Sp/Su)"))),
                      div(column(2, div("Courses")))
                    ))
              )
              renderHeader <<- FALSE


            } else t.out.partial1 <- NULL


            t.out.partial2 <- list(
              div(id=paste0("rowLegacyReport", i),
                  fluidRow(

                    div(column(2, div(summary.info$instructor[i], class=css.courseLoadStatus))),
                    div(column(2, div(paste(AL1, "/", AL2, "/", SL), class=css.courseLoadStatus))),
                    div(column(8, create.textboxes(semester.data, summary.info, i), inline=TRUE))
                  ), class=cssClass
              )
            )
            t.out.partial <- list(t.out.partial1, t.out.partial2)

          }
          t.out.partial
        }

        generateHTML4Report <- function(i, inSummaryInfo) {
          #browser()
          targetInstructor <- inSummaryInfo$Faculty[i]

          if(i==1){
            theHTML <- list(
              div(id="rowLegacyReportHeader",
                  fluidRow(
                    div(column(2, div("Faculty"))),
                    div(column(2, div("Courses Assigned (F/Sp/Su)"))),
                    div(column(2, div("Courses")))
                  ))
            )
          } else {
            theHTML <- NULL
          }
          if(is.na(inSummaryInfo$annualContractedLoad[i])) {
            t.out.partial <- NULL
          } else {
            if(inSummaryInfo$annualContractedLoad[i]==inSummaryInfo$annualAssignedLoad[i]){
              css.courseLoadStatus <- "LegacyReportFacultyOK"
            } else {
              if(inSummaryInfo$annualContractedLoad[i] > inSummaryInfo$annualAssignedLoad[i]){
                css.courseLoadStatus <- "LegacyReportFacultyUnderload"
              } else {
              css.courseLoadStatus <- "LegacyReportFacultyOverload"
              }
              }

            if (i %% 2 == 0){
              cssClass <- "oddRow2"
            } else {
              cssClass <- "evenRow2"
            }
            AL1 <- inSummaryInfo$Fall.assigned[i]
            AL2 <- inSummaryInfo$Spring.assigned[i]
            SL <- inSummaryInfo$Summer.assigned[i]

            t.out.partial2 <- list(
              div(id=paste0("rowLegacyReport", i),
                  fluidRow(

                    div(column(2, div(targetInstructor, class=css.courseLoadStatus))),
                    div(column(2, div(paste(AL1, "/", AL2, "/", SL), class=css.courseLoadStatus))),
                    div(column(8, create.textboxes(semester.data, inSummaryInfo, i), inline=TRUE))
                  ), class=cssClass
              )
            )
            if(is.null(theHTML)){
              theHTML <- list(t.out.partial2)
            } else {
              theHTML <- list(theHTML, t.out.partial2)
            }
          }
          theHTML
        }

        fix.names <- function(data){
          check.duplicate.names <- function(in.data){
            t.substr <- gsub(",.*$", "", in.data)
            t.first <- gsub('.*,\\s*','', in.data)
            t.first <- substr(t.first, 1,1)

            if((t.substr=="Bednarz")|(t.substr=="Guneralp")|(t.substr=="Zhang"))
              t.out <- paste0(t.first, ". ", t.substr)
            else
              t.out <- t.substr
            t.out
          }
          t.out <- lapply(data, check.duplicate.names)
          t.out
        }

        ########################################
        # Report Logic                         #
        ########################################

        semester.code.new <- inSemesterCodes %>%
          mutate(sem.name=paste(semester.chr, Year)) %>%
          filter(sem.name==input$theSemester1) %>%
          mutate(sem5=paste0("20", current.code)) %>%
          select(sem5) %>%
          unlist()

        semester.data <- extractCurrentSemesterData(input$theSemester1)
        academicYearData <- extractAcademicYearData(input$theSemester1)


        if(dim(semester.data)[1] == 0){
          t.out <- h4("No courses are scheduled to be taught this term.  Add courses using the Course Selection tab.", style="color: red")
        } else{
          summary.info <- semester.data %>%
            group_by(instructor, UIN) %>%
            summarise(num.assignments=n(), .groups="drop")

          assign("last.number", 0, pos=1)
          IDs <- seq_len(nrow(semester.data))

          numInstructors <- dim(academicYearData)[1]

          t.out <- lapply(1:(numInstructors), generateHTML4Report, inSummaryInfo=academicYearData)

          t.out.check <- t.out
        }

        displayedInstructors <- dim(academicYearData)[1]
        if(displayedInstructors == 0){
          t.out <- h4(paste0("No instructors have assigned courses for ", input$theSemester1, "."), style="color: red")
        }
        t.out
      })
      output$legacyReportSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("chooseSemesterList1"))
      })

      output$sem <- renderPrint({
        paste("[Report Module] inSemester:", inSemester())
      })

      ########################################
      # Output Options                       #
      ########################################

      outputOptions(output, "chooseSemesterList1", suspendWhenHidden = FALSE)
      outputOptions(output, "legacyReportSidebar", suspendWhenHidden = FALSE)
    }
  )
}
