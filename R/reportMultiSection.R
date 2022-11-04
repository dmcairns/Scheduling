#' reportMultiSectionModuleUI
#'
#' @param id the id
#'
#' @return
#' @export
#'
#' @examples
reportMultiSectionModuleUI <- function(id){
  ns <- NS(id)
  htmltools::div(class="aBoxContainerDiv",
                 shinydashboardPlus::box(
                   title=textOutput(ns("plotTitle")),
                   width=12,
                   collapsible=TRUE,
                   collapsed=TRUE,
                   solidHeader=TRUE,
                   sidebar = boxSidebar(
                     uiOutput(ns("multiSectionSidebar")),
                     width=40,
                     background="#998542",
                     id=ns("legacyReportSidebarControls"),
                     class="testSidebar"
                   ),
                   htmltools::div(class="aBoxBodyC3Background",
                                  fluidRow(
                                    column(12, uiOutput(ns("multisectionReport")))
                                  )
                   ),
                   id=ns("legacyReportBox1"),
                   class="schBoxTestOct"
                 ))

}

#' reportMultiSectionModuleServer
#'
#' @param id the id
#' @param input the input
#' @param output the output
#' @param session the session
#' @param inSemester input semester
#' @param theMasterCourses the master course list
#' @param theCombinedData the combined data
#' @param theLeaveData the leave data
#' @param inSemesterCodes the semester codes
#'
#' @return
#' @export
#'
#' @examples
reportMultiSectionModuleServer <- function(id, input, output, session, inSemester, theMasterCourses,
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
      output$courseSelection <- renderUI({
        ns <- session$ns
        courseList <- theMasterCourses() %>%
          select("courseID") %>%
          unique() %>%
          unlist() %>%
          as.vector()
        selectInput(ns("theCourse"), "Course Selection:", choices=courseList)
      })
      #############################
      # Card Elements             #
      #############################
      output$plotTitle <- renderText({
        theTitle <- paste0("Multi-section Report for ", input$theCourse)
        theTitle
      })
      output$multisectionReport <- renderUI({
        req(theMasterCourses())
        req(input$theCourse)
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
                column(2, column.headers[i], class="MultiSectionReportHeader")
              )
            } else {
              t.out[[1]] <- tagAppendChild(t.out[[1]], column(2, column.headers[i], class="MultiSectionReportHeader"))
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
                  course.class <- "FacultyReportCourseOnline"
                }
                else {
                  course.class <- "FacultyReportCourse"
                }
                column(2, div(data[i,1], class=course.class))
              } else {
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
            summarise(n=n()) %>%
            select(n) %>%
            unlist() %>%
            max(na.rm=T)

          output.matrix <- matrix(nrow=course.count, ncol=num.semesters, data=NA)
          semesters <- unique(data1$semester)
          for(i in 1:num.semesters){
            instructors <- data1 %>%
              filter(semester==semesters[i]) %>%
              select(instructor) %>%
              unlist()
            output.matrix[c(1:length(instructors)),i] <- instructors
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
            summarise(n=n()) %>%
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

          output.matrix
        }
        # select the facutly
        multi.section.courses <- theMasterCourses() %>%
          filter(courseID == input$theCourse)
        num.semesters <- multi.section.courses %>%
          select(semester) %>%
          unique() %>%
          unlist() %>%
          length()

        text.matrix <- create.text.matrix(multi.section.courses, num.semesters)
        online.matrix <- create.online.matrix(multi.section.courses, num.semesters)
        num.sections <- ceiling(num.semesters / 6)

        for(k in 1:num.sections){
          start.pos <- (k-1)*6+1
          end.pos <- start.pos+5
          if(end.pos > num.semesters) end.pos <- num.semesters
          t.partial <- create.header(multi.section.courses, start.pos, end.pos)
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
      output$multiSectionSidebar <- renderUI({
        ns <- session$ns
        uiOutput(ns("courseSelection"))
      })

      outputOptions(output, "courseSelection", suspendWhenHidden = FALSE)
      outputOptions(output, "multiSectionSidebar", suspendWhenHidden = FALSE)
    }
  )
}
