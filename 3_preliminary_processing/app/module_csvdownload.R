csvdownload_UI <- function(id, tag = 'Download CSV') {
  uiOutput(NS(id, "dl_interface"))
  tagList(
    downloadButton(
      NS(id, 'download'),
      tag
    )
  )
}

csvdownload_server <- function(id, filename, payload) {
  moduleServer(
    id, function(input, output, session) {
      output$download <- downloadHandler(
        filename = function(name = filename) {
          return(name)
        },
        content = function(file){
          write.csv(
            payload(),
            file,
            quote = TRUE
          )
        }
      )
    }
  )
}
