ggdownload_UI <- function(id) {
  uiOutput(NS(id, "dl_interface"))
  tagList(
    selectInput(
      NS(id, 'filetype'),
      "File type for download: ",
      choices = c("pdf", "png"),
      selected = "png",
      multiple = FALSE
    ),
    numericInput(
      NS(id, 'height'),
      "Output height:",
      min = 1000,
      max = 8000,
      step = 500,
      value = 2400
    ),
    numericInput(
      NS(id, 'width'),
      "Output width:",
      min = 1500,
      max = 10000,
      step = 500,
      value = 3000
    ),
    downloadButton(
      NS(id, 'download'),
      'Download'
    )
  )
}

ggdownload_server <- function(id, filename, payload) {
  moduleServer(
    id, function(input, output, session) {
      output$download <- downloadHandler(
        filename = function(gene = filename, ext = input$filetype) {
          if (length(gene) == 0) {
            gene <- "download"
          }
          if (length(gene) > 1) {
            gene <- paste(gene, collapse = "_")
          }
          gene <- gsub("[[:punct:] ]+", "_", gene)
          return(paste(gene, ext, sep = "."))
        },
        content = function(file){
          ggsave(
            file,
            plot = payload(),
            height = input$height,
            width = input$width,
            limitsize = FALSE,
            units = "px"
          )
        }
      )
    }
  )
}
