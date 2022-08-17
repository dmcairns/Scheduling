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
                                    column(12, uiOutput(ns("legacyReport1")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

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
        req(inSemester())     #May not be necessary
        req(theMasterCourses())
        req(theCombinedData())
        ns <- session$ns
        renderHeader <- TRUE
        displayedInstructors <- 0

        # t.assignedLoads <- theCombinedData() %>%
        #   filter(longSemester==inSemester()) %>%
        #   filter(shortNameNoSpace %in% c("Bishop", "Cairns", "Jepson", "Ewers")) %>%
        #   select("shortNameNoSpace", "longSemester", "load", "assigned.load")
        #
        # print(t.assignedLoads)

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
          courses.found <- find.courses(data, data.summary, i)
          numCourses <- dim(courses.found)[1]
          t.out <- lapply(1:(numCourses), function(j) {
            extract.course(courses.found, j)
          })

          if(is.na(t.out[[1]][2])) {
            #cat("***********************In is.na(t.out):\n")
            instructor.of.interest <- data.summary[i,"instructor"]
            instructor.of.interest <- as.vector(unlist(instructor.of.interest))

            # if load for instructor in the selected semester is zero,
            # then there needs to be a reason for it.  Otherwise, the lack of
            # courses just means that they haven't been assigned yet.


            t.load <- theCombinedData() %>%
              filter(shortName == instructor.of.interest) %>%
              #filter(longSemester == inSemester()) %>%
              filter(longSemester == input$theSemester1) %>%
              select("Faculty"="shortName", "semester"="longSemester", "load") %>%
              mutate(load = case_when(
                is.na(load) ~ 0,
                TRUE ~ load
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
        #combine t.summary.info and available faculty to get zero loads.
        # select the appropriate semester from  available.faculty
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

        #t.sem <- inSemester()
        req(input$theSemester1)
        t.sem <- input$theSemester1
        semester.code.new <- semester.codes %>%
          mutate(sem.name=paste(semester.chr, Year)) %>%
          filter(sem.name==t.sem) %>%
          mutate(sem5=paste0("20", current.code)) %>%
          select(sem5) %>%
          unlist()


        #get the data for the specified semester
        semester.data <- theMasterCourses() %>%
          #filter(semester==inSemester())
          filter(semester==input$theSemester1)

        if(dim(semester.data)[1] == 0){
          t.out <- h4("No courses are scheduled to be taught this term.  Add courses using the Course Selection tab.", style="color: red")
        } else{
          summary.info <- semester.data %>%
            group_by(instructor) %>%
            summarise(num.assignments=n(), .groups="drop")

          summary.info <- theCombinedData() %>%
            #filter(longSemester == inSemester()) %>%
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

          t.summary.info <<- summary.info



          assign("last.number", 0, pos=1)
          IDs <- seq_len(nrow(semester.data))
          numInstructors <- dim(summary.info)[1]

          t.out <- lapply(1:(numInstructors), function(i) {
            # determine load level (OK, overload, underload), assign to css.courseLoadStatus

            contracted.load <- summary.info %>%
              filter(instructor==t.summary.info$instructor[i]) %>%
              select(load) %>%
              unlist()
            modified.assigned.load <- summary.info %>%
              filter(instructor==summary.info$instructor[i]) %>%
              select(assigned.load) %>%
              unlist()

            #annual.load <- calc.annual.courses.assigned.including.stacked.New(t.summary.info$instructor[i], inSemester())
            annual.load <- calc.annual.courses.assigned.including.stacked.New(t.summary.info$instructor[i], input$theSemester1)
            if(is.na(contracted.load)) {
              # do not include this faculty member in the report
              #contracted.load <- 0
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
              fallOrSpring <- semester.codes %>%
                #filter(longSemester==inSemester()) %>%
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
          t.out.check <<- t.out
        } #end of else checking to see that there are data

        if(displayedInstructors == 0){
          #t.out <- h4(paste0("No instructors have assigned courses for ", inSemester(), "."), style="color: red")
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
      output$legacyReportSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("chooseSemesterList1"))
      })

      output$sem <- renderPrint({
        paste("[Report Module] inSemester:", inSemester())
      })

      outputOptions(output, "chooseSemesterList1", suspendWhenHidden = FALSE)
      outputOptions(output, "legacyReportSidebar", suspendWhenHidden = FALSE)
    }
  )
}
