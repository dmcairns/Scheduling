reportFacultyModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("facultyReportSidebar")),
                     width=40,
                     background="#998542",
                     id=ns("legacyReportSidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("facultyReport")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

reportFacultyModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
                                      theCombinedData, theLeaveData, inSemesterCodes,
                                      inSelectedFaculty, allowUpdate=FALSE, rds.path=NULL){
  moduleServer(
    id,
    function(input, output, session){
      #cat(yellow("[reportFacultyModuleServer]"), green(inSelectedFaculty), ".\n")
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
      observeEvent(input$theFacultyMember,{
        localSelectedFaculty <- input$theFacultyMember
      })
      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Individual Faculty Report for ", input$theFacultyMember)
        #theTitle <- paste0("Individual Faculty Report for ", inSelectedFaculty())

        theTitle
      })
      output$facultyReport <- renderUI({
        ns <- session$ns
        #req(theMasterCourses)
        req(input$theFacultyMember)
        create.header <- function(data, pos.start, pos.end){
          sem.codes <- inSemesterCodes %>%
            mutate(full.semester=paste(semester.chr, Year))

          column.headers <- data %>%
            left_join(sem.codes, by=c("semester"="full.semester")) %>%
            arrange(current) %>%
            select(semester) %>%
            unique() %>%
            unlist
          column.headers <- column.headers[pos.start:pos.end]

          t.out <- vector("list", 1)
          for(i in 1:length(column.headers)){
            if(i==1){
              t.out[[1]] <- fluidRow(
                column(2, column.headers[i], class="FacultyReportHeader")
              )
            } else {
              t.out[[1]] <- tagAppendChild(t.out[[1]], column(2, column.headers[i], class="FacultyReportHeader"))
            }

          }
          t.out
        }
        create.grid <- function(data,m.online, pos.start,pos.end){

          num.rows <- nrow(data)

          data <- data[,c(pos.start:pos.end)]



          m.online <- m.online[,c(pos.start:pos.end)]
          if(pos.start==pos.end)
            data <- matrix(nrow=length(data), ncol=1, data=data)
          data <- matrix(data, nrow=num.rows, ncol=(pos.end-pos.start+1))
          m.online <- matrix(m.online, nrow=num.rows, ncol=(pos.end-pos.start+1))

          t.out <- vector("list", num.rows)

          for(i in 1:num.rows){
            num.cols <- ncol(data)

            t.out[[i]] <- fluidRow(
              if(!is.na(data[i,1])) {
                if(is.null(dim(m.online))) {
                  if(m.online[i]==TRUE){
                    course.class <- "FacultyReportCourseOnline"
                  } else {

                    course.class <- "FacultyReportCourse"
                  }
                } else if(m.online[i,1]==TRUE) {
                  # cat("In the else\n")
                  course.class <- "FacultyReportCourseOnline"
                }
                else {
                  course.class <- "FacultyReportCourse"
                }
                column(2, div(data[i,1], class=course.class))
              } else {
                # cat("in the else\n")
                column(2,"")
              }
            )
            if(num.cols>1){
              for(j in 2:num.cols){
                if(!is.na(data[i,j])){
                  if(m.online[i,j]==TRUE) course.class <- "FacultyReportCourseOnline"
                  else course.class <- "FacultyReportCourse"
                  t.out[[i]] <- tagAppendChild(t.out[[i]], column(2, div(data[i,j], class=course.class)))
                } else {
                  t.out[[i]] <- tagAppendChild(t.out[[i]], column(2,""))
                }
              }
            }
          }

          t.out
        }
        create.text.matrix <- function(data, num.semesters){

          sem.codes <- inSemesterCodes %>%
            mutate(full.semester=paste(semester.chr, Year))
          data1 <- data %>%
            left_join(sem.codes, by=c("semester"="full.semester")) %>%
            arrange(current) %>%
            unique()

          course.count <- data1 %>%
            select(semester, courseID, OL) %>%
            group_by(semester) %>%
            summarise(n=n(), .groups="drop") %>%
            select(n) %>%
            unlist() %>%
            max(na.rm=T)

          output.matrix <- matrix(nrow=course.count, ncol=num.semesters, data=NA)
          semesters <- unique(data1$semester)
          for(i in 1:num.semesters){
            courses <- data1 %>%
              filter(semester==semesters[i]) %>%
              select(courseID) %>%
              unlist()

            output.matrix[c(1:length(courses)),i] <- courses
          }


          output.matrix
        }
        create.online.matrix <- function(data, num.semesters){

          sem.codes <- inSemesterCodes %>%
            mutate(full.semester=paste(semester.chr, Year))
          data1 <- data %>%
            left_join(sem.codes, by=c("semester"="full.semester")) %>%
            arrange(current) %>%
            unique()

          course.count <- data1 %>%
            select(semester, courseID, OL) %>%
            group_by(semester) %>%
            summarise(n=n(), .groups="drop") %>%
            select(n) %>%
            unlist() %>%
            max(na.rm=T)

          output.matrix <- matrix(nrow=course.count, ncol=num.semesters, data=NA)
          semesters <- unique(data1$semester)
          for(i in 1:num.semesters){
            courses <- data1 %>%
              filter(semester==semesters[i]) %>%
              select(OL) %>%
              unlist()
            output.matrix[c(1:length(courses)),i] <- courses
          }

          #print(output.matrix)
          output.matrix
        }
        # select the facutly
        #assign("t.mc3000", rv$master.courses, pos=1)
        single.faculty.courses <- theMasterCourses() %>%
          filter(instructor == input$theFacultyMember)
        num.semesters <- single.faculty.courses %>%
          select(semester) %>%
          unique() %>%
          unlist() %>%
          length()


        text.matrix <- create.text.matrix(single.faculty.courses, num.semesters)

        online.matrix <- create.online.matrix(single.faculty.courses, num.semesters)

        num.sections <- ceiling(num.semesters / 6)

        for(k in 1:num.sections){
          start.pos <- (k-1)*6+1
          end.pos <- start.pos+5
          if(end.pos > num.semesters) end.pos <- num.semesters
          t.partial <- create.header(single.faculty.courses, start.pos, end.pos)

          if(k==1){
            t.out <- t.partial
            t.out <- c(t.out, create.grid(text.matrix, online.matrix, start.pos, end.pos))

          } else {
            t.out <- c(t.out, t.partial)
            t.out <- c(t.out, create.grid(text.matrix, online.matrix, start.pos, end.pos))
          }
        }

        t.out
      })
      output$facultyReportSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("chooseFaculty"))
      })

      outputOptions(output, "chooseFaculty", suspendWhenHidden = FALSE)
      outputOptions(output, "facultyReportSidebar", suspendWhenHidden = FALSE)
      return(localSelectedFaculty)
    }
  )
}
