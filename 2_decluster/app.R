#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ROI merger"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("cuth",
                        "Merging diameter:",
                        min = 1,
                        max = 30,
                        value = 10),
            fileInput("upload", "Upload a file"),
            uiOutput("downloadbtn")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("eyePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  coord_table <- reactiveValues(df = NULL, merged = NULL)
  observeEvent(
    input$upload, {
      output$downloadbtn <- renderUI(
        {
          downloadButton(outputId = "get_file", label = "download")
        }
      )
      return(
        coord_table$df <- read.csv(input$upload$datapath, row.names = 1)
      )
    }
  )

    observeEvent(
      input$upload,
      {
        coordplot <- ggplot(coord_table$df, aes(x = X, y = Y)) +
          geom_point() +
          theme_bw() +
          scale_x_reverse()
        output$eyePlot <- renderPlot(coordplot)
      }
    )

    observeEvent(
      input$cuth,
      {
        if (!is.null(input$upload)) {
          curr_var <- coord_table$df
          curr_var$mer <- cutree(
          hclust(dist(curr_var[, c("X", "Y")])),
          h = input$cuth
          )
          new_var <- data.frame(
            X = tapply(curr_var$X, curr_var$mer, mean),
            Y = tapply(curr_var$Y, curr_var$mer, mean)
          )
          coord_table$merged <- new_var
          coordplot <- ggplot(new_var, aes(x = X, y = Y)) +
            geom_point() +
            theme_bw() +
            scale_x_reverse()
          output$eyePlot <- renderPlot(coordplot)
        }
      }
    )

    output$get_file <- downloadHandler(
      filename = function() {
        paste0("Results_merged_", input$cuth, ".csv")
      },
      content = function(file) {
        write.csv(
          coord_table$merged,
          file,
          row.names = FALSE,
          quote = FALSE
        )
      }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
