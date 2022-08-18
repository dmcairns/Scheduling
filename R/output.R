#' outputModuleUI
#'
#' @param id the id
#'
#' @return
#' @export
#'
#' @examples
outputModuleUI <- function(id){
  wellPanel(h3("Output Module"),
            verbatimTextOutput(NS(id, "txt")))

}

#' outputModuleServer
#'
#' @param id the id
#' @param input the input
#' @param output the output
#' @param session the session
#' @param ImProxy the improxy
#'
#' @return
#' @export
#'
#' @examples
outputModuleServer <- function(id, input, output, session, ImProxy){
  moduleServer(
    id,
    function(input, output, session){
      output$txt <- renderPrint({
        paste("nrow(ImProxy):", nrow(ImProxy()), "\n")

      })
    }
  )
}
