outputModuleUI <- function(id){
  wellPanel(h3("Output Module"),
            verbatimTextOutput(NS(id, "txt")))

}

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
