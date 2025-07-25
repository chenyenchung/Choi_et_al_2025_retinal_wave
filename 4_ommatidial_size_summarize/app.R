#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
options(bitmapType = 'cairo')
required <- c(
  "shiny", "readxl", "tidyr",
  "dplyr", "area", "viridisLite",
  "ggplot2", "patchwork", "RColorBrewer"
)

for (package in required) {
  if (!require(package, character.only = TRUE)) {
    install.packages(
      package,
      repos = "https://cloud.r-project.org"
    )
  }
}

library(shiny)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(area)
library(viridisLite)
library(patchwork)
library(RColorBrewer)

options(shiny.maxRequestSize = 1024^3)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ommatididal distance pre-process"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "controls",
            tabPanel(
              "Upload",
              selectInput(
                "filetype", "Please select a file format",
                choices = c("xls", "xlsx", "csv"), selected = "csv"
              ),
              numericInput(
                "skip", "How many rows are there before the column names?",
                value = 0, min = 0, max = NA
              ),
              fileInput("upload", "Upload a file")
            ),
            tabPanel(
              "Settings",
              checkboxInput(
                "chk_showid",
                "Show Object ID"
              ),
              numericInput(
                "scale_factor", "Length of Lines for Preview Plots",
                min = 0.1, value = 1.5
              ),
              numericInput(
                "dist_thres", "How far away should we search for 6 neighbors?",
                value = 20, min = 0, max = NA
              ),
              numericInput(
                "bin_num", label = "Roughly, how many bins per axis?",
                value = 10, min = 2, max = NA, step = 1
              ),
              uiOutput("center_select"),
              uiOutput("d_select"),
              uiOutput("v_select"),
              uiOutput("a1_select"),
              uiOutput("p1_select"),
              uiOutput("a2_select"),
              uiOutput("p2_select"),
              uiOutput("preprocess_btn")
            )
          )
        ),

        mainPanel(
          tabsetPanel(
            id = "outputs",
            tabPanel(
              "Input Preview",
              tableOutput("preview_tbl")
            ),
            tabPanel(
              "Embedding Preview",
              uiOutput("preview_btn"),
              uiOutput("download_btn"),
              plotOutput("aligned_eye")
            ),
            tabPanel(
              "Plot Preview",
              plotOutput("plot_preview")
            ),
            tabPanel(
              "Download Links",
              uiOutput("din_avg_dist"),
              uiOutput("din_ind_dist"),
              uiOutput("din_axis_angle"),
              uiOutput("din_axis_asymmetry")
            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Make uploaded data reactive
  shared_df <- reactiveValues(df = NULL, id = NULL)

  observeEvent(
    input$upload, {
      # Try to load the input. Show a message if failed.
      tryCatch(
        {
          # Try to read the table
          if (input$filetype == "xls") {
            center_df <- read_xls(input$upload$datapath, skip = input$skip) |>
              as.data.frame()
          } else if (input$filetype == "xlsx") {
            center_df <- read_xlsx(input$upload$datapath, skip = input$skip) |>
              as.data.frame()
          } else {
            center_df <- read.csv(input$upload$datapath, skip = input$skip)
          }
          # Ensure that coordinates are read correctly
          if (any(apply(center_df[ , 1:3], 2, class) != "numeric")) {
            showNotification(
              "Please examine the first 3 columns. ",
              "There should only be numbers except for the first row."
            )
          }

          # Ensure that we have annotation
          if (!"match" %in% colnames(center_df)) {
            showNotification(
              "Axis annotation is expected in a column named 'match'. ",
              "Please note that column names are case-sensitive."
            )
          } else {
            # If a table is loaded successfully, format it to make it easier to use
            row.names(center_df) <- center_df$ID

            # Keep center IDs for later use
            center_id <- center_df$ID
            colnames(center_df)[1:3] <- c("x", "y", "z")

            # Rename the annotation column content to avoid NAs
            center_df$match[is.na(center_df$match)] <- "other"
            center_df$match[center_df$match == ""] <- "other"

            # Update shared reactive object
            shared_df$df <- center_df
            shared_df$id <- center_id

            # Switch to the setting tab for the next step
            updateTabsetPanel(
              session = session,
              inputId = "controls",
              selected = "Settings"
            )

            # Render selection interface for reference points
            ann_classes <- setdiff(unique(center_df$match), "other")
            output$center_select <- renderUI(
              {
                selectInput(
                  "center_label",
                  label = "What is the label for the center point?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("center", ann_classes, ignore.case = TRUE),
                    grep("center", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$v_select <- renderUI(
              {
                selectInput(
                  "v_label",
                  label = "What is the label for the ventral point?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("dvv", ann_classes, ignore.case = TRUE),
                    grep("dvv", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$d_select <- renderUI(
              {
                selectInput(
                  "d_label",
                  label = "What is the label for the dorsal point?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("dvd", ann_classes, ignore.case = TRUE),
                    grep("dvd", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$a1_select <- renderUI(
              {
                selectInput(
                  "a1_label",
                  label = "What is the label for the anterior point #1?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("ava", ann_classes, ignore.case = TRUE),
                    grep("ava", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$p1_select <- renderUI(
              {
                selectInput(
                  "p1_label",
                  label = "What is the label for the posterior point #1?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("avp", ann_classes, ignore.case = TRUE),
                    grep("avp", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$a2_select <- renderUI(
              {
                selectInput(
                  "a2_label",
                  label = "What is the label for the anterior point #2?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("ada", ann_classes, ignore.case = TRUE),
                    grep("ada", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
            output$p2_select <- renderUI(
              {
                selectInput(
                  "p2_label",
                  label = "What is the label for the posterior point #2?",
                  choices = ann_classes,
                  selected = ifelse(
                    grepl("adp", ann_classes, ignore.case = TRUE),
                    grep("adp", ann_classes, ignore.case = TRUE, value = TRUE),
                    ann_classes[1]
                  )
                )
              }
            )
          }

          # Ensure that we have IDs
          if (!"ID" %in% colnames(center_df)) {
            showNotification(
              "Ommatidium ID is expected in a column named 'ID'. ",
              "Please note that column names are case-sensitive."
            )
          } else {
            output$preprocess_btn <- renderUI(
              {actionButton("run_pre", label = "Start to Process")}
            )
          }
        },
        error = function(cond) {
          showNotification(
            "Fail to load the table. Is a correct file type selected?"
          )
          showNotification(as.character(cond))
        }
      )
      if (exists("center_df")) {
        output$preview_tbl <- renderTable(center_df)
      } else {
        output$preview_tbl <- renderTable(
          data.frame(Error = "Fail to load the uploaded file.")
        )
      }
    }
  )

  observeEvent(
    input$run_pre, {
      scale_factor <- input$scale_factor
      center_df <- shared_df$df
      center_id <- shared_df$id

      # Calculate 3D distance in original space
      pt_dist <- dist(center_df[ , c("x", "y", "z")]) |>
        as.matrix() |>
        as.data.frame()

      row.names(pt_dist) <- center_id
      colnames(pt_dist) <- center_id

      nn_avgdist <- apply(pt_dist, 2, function(x) {
        to_keep <- x < input$dist_thres
        return(median(sort(x[to_keep], decreasing = FALSE)[2:min(6, sum(to_keep))]))
      })

      nn_distdf_raw <- lapply(1:ncol(pt_dist), function(x) {
        dist_df <- data.frame(
          dist = sort(pt_dist[ , x])[2:7],
          id1 = colnames(pt_dist)[x],
          id2 = row.names(pt_dist)[order(pt_dist[ , x])[2:7]]
        )
        return(dist_df)
      })

      ## Only keep the ommatidia that have 6 neighbors within a given distance.
      nn_distdf_raw <- do.call(rbind, nn_distdf_raw) |> as.data.frame()
      nn_distdf <- subset(nn_distdf_raw, dist < input$dist_thres)

      nn_keep <- table(nn_distdf$id1)
      nn_keep <- names(nn_keep[nn_keep > 2])

      nn_avgdist <- nn_avgdist[nn_keep]
      nn_distdf <- nn_distdf %>% filter(id1 %in% nn_keep)

      # Embed remaining ommatidia onto a PC space
      pca_emb_raw <- prcomp(center_df[ , 1:3])$x[ , 1:2] |> as.data.frame()
      pca_emb_raw$ann <- center_df[ , "match", drop = TRUE]
      pca_emb_raw$center_id <- center_id

      # Align PCs to anatomical landmarks
      ## Center the center
      pca_emb_raw[ , 1:2] <- sweep(
        pca_emb_raw[ , 1:2], 2,
        unlist(pca_emb_raw[pca_emb_raw$ann == input$center_label, 1:2]), `-`
      )

      ## Get V -> D axis
      dv_vec <- unlist(pca_emb_raw[pca_emb_raw$ann == input$d_label, 1:2] - pca_emb_raw[pca_emb_raw$ann == input$v_label, 1:2])

      # Rotate so D points to the top
      rotate_radian <- pi/2 - atan2(y = dv_vec[2], x = dv_vec[1])
      rot_mat <- matrix(
        c(
          cos(rotate_radian), sin(rotate_radian),
          -1 * sin(rotate_radian), cos(rotate_radian)
        ), nrow = 2

      )

      pca_emb_rot <- pca_emb_raw
      pca_emb_rot[ , 1:2] <- t(rot_mat %*% t(as.matrix(pca_emb_raw[ , 1:2])))

      # Ensure A-P relationship
      apvec1 <- unlist(pca_emb_rot[pca_emb_rot$ann == input$p1_label, 1:2] - pca_emb_rot[pca_emb_rot$ann == input$a1_label, 1:2])
      apvec2 <- unlist(pca_emb_rot[pca_emb_rot$ann == input$p2_label, 1:2] - pca_emb_rot[pca_emb_rot$ann == input$a2_label, 1:2])
      if (apvec1[1] * apvec2[1] < 0) {
        showNotification(
          "Please note that the relative positions of ",
          "two pairs of A-P reference point are incoherent."
        )
      }
      if (apvec1[1] > 0 & apvec2[1] > 0) {
        pca_emb_rot$PC1 <- pca_emb_rot$PC1 * -1
      }

      row.names(pca_emb_rot) <- pca_emb_rot$center_id
      pca_emb_rot$PC1 <-  pca_emb_rot$PC1 * 100 / (max(pca_emb_rot$PC1) - min(pca_emb_rot$PC1))
      pca_emb_rot$PC2 <-  pca_emb_rot$PC2 * 100 / (max(pca_emb_rot$PC2) - min(pca_emb_rot$PC2))
      bin_size <- 100 / (input$bin_num - 1)
      pca_emb_rot$bin_id <- paste0(
        "(", ceiling(pca_emb_rot$PC1 / bin_size), ", ",
        ceiling(pca_emb_rot$PC2 / bin_size), ")"
      )

      # Extract coordinates from rotated PC space for each neighboring pairs
      nn_distdf <- apply(nn_distdf, 1, function(x) {
        o1 <- pca_emb_rot[x[["id1"]], 1:2]
        names(o1) <- c("x1", "y1")
        o2 <- pca_emb_rot[x[["id2"]], 1:2]
        names(o2) <- c("x2", "y2")
        bin_id <- pca_emb_rot[x[["id1"]], "bin_id"]

        dir_vecs <- o2 - o1

        # Scale distance to neighbors to a fixed number so we can plot
        # spaced hex grid later. This (o2) is for visualization only -- the
        # actual distance is in the dist column.
        o2 <- (scale_factor * dir_vecs / sqrt(sum((o2 - o1)^2))) + o1
        out_df <- data.frame(
          "ID1" = x[["id1"]], o1,
          "ID2" = x[["id2"]], o2,
          "dist" = as.numeric(x[["dist"]]),
          bin_id
        )
        return(out_df)
      })
      nn_distdf <- do.call(rbind, nn_distdf) |> as.data.frame()

      ## Order neighbors by clock-wise
      nn_distdf <- split(nn_distdf, nn_distdf$ID1)

      nn_distdf <- lapply(nn_distdf, function(x) {
        if (nrow(x) < 6)  {return(NULL)}
        atan_vecs <- atan2(y = (x[["y2"]] - x[["y1"]]), x = (x[["x2"]] - x[["x1"]]))
        rad_order <- order(atan_vecs, decreasing = TRUE)
        return(x[rad_order, ])
      })

      nn_distdf <- nn_distdf[sapply(nn_distdf, function(x) {return(!is.null(x))})]

      nn_distdf <- do.call(rbind, nn_distdf) |> as.data.frame()


      sqrt_avgdist <- nn_avgdist
      nn_all_6 <- unique(nn_distdf$ID1)
      names(nn_all_6) <- nn_all_6

      omm_est_area <- vapply(
        nn_all_6, function(id) {
          neighbors <- subset(nn_distdf, ID1 == id)$ID2
          coord <- subset(center_df, ID %in% c(neighbors, id))[ , c("ID", "x", "y", "z")]
          row.names(coord) <- coord$ID
          coord <- coord[, c("x", "y", "z")]
          pc <- prcomp(coord)$x[ , 1:2]
          pc <- pc[neighbors, ]

          atan_vecs <- atan2(y = pc[, 2], x = pc[, 1])
          rad_order <- order(atan_vecs, decreasing = TRUE)
          pc <- pc[rad_order, ]
          scale_coef <-  sqrt_avgdist[id] / (sqrt_avgdist[id] + sqrt_avgdist[row.names(pc)])
          pc <- sweep(pc, 1, scale_coef, `*`)
          return(polygon_area(pc))
        }, FUN.VALUE = numeric(1)
      )


      ## Calculate diagonal distances
      diag_sum <- nn_distdf
      diag_sum$pair <- rep(1:3, nrow(diag_sum) / 3)
      diag_sum <- diag_sum %>%
        group_by(ID1, bin_id, pair) %>%
        summarise(
          nx1 = mean(x1), ny1 = mean(y1),
          x2 = diff(x2) * sum(dist),
          y2 = diff(y2) * sum(dist)
        ) %>%
        group_by(ID1, bin_id) %>%
        summarize(x1 = mean(nx1), y1 = mean(ny1), dx = sum(x2), dy = sum(y2))
      diag_sum$degree <- atan(diag_sum$dy/diag_sum$dx) / pi * 180

      ## Calculate diagonal asymmetry
      diag_diff <- nn_distdf
      diag_diff$pair <- rep(1:3, nrow(diag_diff) / 3)
      diag_diff <- diag_diff %>%
        group_by(ID1, bin_id, pair) %>%
        summarise(
          nx1 = mean(x1), ny1 = mean(y1),
          x2 = diff(x2) * diff(dist),
          y2 = diff(y2) * diff(dist),
          dist_diff = abs(diff(dist))
        )
      diag_diff$degree <- atan2(diag_diff$y2, diag_diff$x2)

      colpal <- c("grey92", viridis(length(unique(pca_emb_rot$ann)) - 1))
      names(colpal) <- c("other", setdiff(unique(pca_emb_rot$ann), "other"))

      p <- pca_emb_rot %>%
        ggplot(aes(x = PC1, y = PC2, color = ann)) +
        geom_point(size = 5) +
        annotate(geom = "segment", x = 10, xend = 50, y = -30, yend = -30,
                 arrow = arrow(length = unit(3, "pt"))) +
        annotate(geom = "segment", x = 30, xend = 30, y = -50, yend = -10,
                 arrow = arrow(length = unit(3, "pt"))) +
        annotate(geom = "text", x = 30, y = -5, label = "D", fontface = "bold") +
        annotate(geom = "text", x = 54, y = -30, label = "A", fontface = "bold") +
        theme_minimal() +
        labs(color = "Annotated Ref Points", title = "Reference Point") +
        scale_color_manual(values = colpal)

      if (input$chk_showid) {
        p <- p + geom_text(aes(label = center_id), color = "black")
      }

      binpal <- rep(brewer.pal(8, "Dark2"),
                    floor(length(unique(pca_emb_rot$bin_id)) / 8) + 1)
      bin_p <- pca_emb_rot %>%
        ggplot(aes(x = PC1, y = PC2, color = bin_id)) +
        geom_point(size = 5) +
        annotate(geom = "segment", x = 10, xend = 50, y = -30, yend = -30,
                 arrow = arrow(length = unit(3, "pt"))) +
        annotate(geom = "segment", x = 30, xend = 30, y = -50, yend = -10,
                 arrow = arrow(length = unit(3, "pt"))) +
        annotate(geom = "text", x = 30, y = -5, label = "D", fontface = "bold") +
        annotate(geom = "text", x = 54, y = -30, label = "A", fontface = "bold") +
        theme_minimal() +
        labs(title = "Binning map") +
        guides(color = "none") +
        scale_color_manual(values = binpal)

      output$aligned_eye <- renderPlot((p / bin_p), width = 800, height = 1200)

      # Prepare output
      outdf <- cbind(center_df[nn_keep, c("x", "y", "z")], pca_emb_rot[nn_keep, ])
      outdf$med_dist <- nn_avgdist
      outdf$est_area <- omm_est_area[row.names(outdf)]
      out_diag_diff <- diag_diff[ , c("ID1", "nx1", "ny1", "x2", "y2", "dist_diff", "degree", "bin_id")]
      colnames(out_diag_diff)[1:5] <- c("ID", "x1", "y1", "dx", "dy")

      shared_df$avg_dist <- outdf
      shared_df$ind_dist <- nn_distdf
      shared_df$axis_asymmetry <- out_diag_diff
      shared_df$axis_angle <- diag_sum

      output$preview_btn <- renderUI({
        actionButton("plot_preview_btn", "Draw Plots for Preview")
      })
      output$download_btn <- renderUI({
        actionButton("make_dl_btn", "Prepare Tables for Download")
      })
      updateTabsetPanel(
        inputId = "outputs",
        session = session,
        selected = "Embedding Preview"
      )
    }
  )

  observeEvent(
    input$plot_preview_btn, {
      pca_emb_rot <- shared_df$avg_dist
      nn_distdf <- shared_df$ind_dist
      diag_sum <- shared_df$axis_angle
      diag_diff <- shared_df$axis_asymmetry


      p <- pca_emb_rot %>%
        ggplot(aes(x = PC1, y = PC2)) +
        geom_point(aes(color = med_dist), size = 3) +
        scale_color_gradient2(
          low = "blue", high = "red",
          midpoint = (max(pca_emb_rot$med_dist) + min(pca_emb_rot$med_dist)) / 2
        ) +
        labs(
          color = "Median Neighbor Distance",
          title = "Median distance with neighboring ommatidia"
        ) +
        theme_minimal()

      pa <- pca_emb_rot %>%
        filter(!is.na(est_area)) %>%
        ggplot(aes(x = PC1, y = PC2)) +
        geom_point(aes(color = est_area), size = 3) +
        scale_color_gradient2(
          low = "blue", high = "red",
          midpoint = (max(pca_emb_rot$est_area, na.rm = TRUE) + min(pca_emb_rot$est_area, na.rm = TRUE)) / 2
        ) +
        labs(
          color = "Area (Sq. pixel)",
          title = "Estimated Ommatidial Area"
        ) +
        theme_minimal()

      ppi <- nn_distdf %>%
        ggplot(aes(x = x1, y = y1)) +
        # geom_polygon(aes(x = x2, y = y2, group = ID1), color = "black", fill = "orange", alpha = 0.3) +
        geom_segment(aes(xend = x2, yend = y2, color = dist), linewidth = 1) +
        scale_color_gradient2(
          low = "darkblue", high = "darkred",
          midpoint = (max(nn_distdf$dist) + min(nn_distdf$dist)) / 2
        ) +
        labs(
          title = "Distance with individual neighboring ommatidia",
          color = "Individual distance", x = "PC1", y = "PC2"
        ) +
        theme_minimal()

      aop <- diag_sum %>%
        ggplot(aes(x = x1, y = y1)) +
        geom_polygon(
          data = nn_distdf,
          aes(x = x2, y = y2, group = ID1), color = "black",
          fill = "orange", alpha = 0.3
        ) +
        geom_segment(
          aes(
            x = x1 - 5e-3 * dx, xend = x1 + 5e-3 * dx,
            y = y1 - 5e-3 * dy, yend = y1 + 5e-3 * dy
          ),
          linewidth = 0.5
        ) +
        labs(title = "Axis orientation", x = "PC1", y = "PC2") +
        theme_minimal()


      asp <- diag_diff %>%
        ggplot(aes(x = x1, y = y1)) +
        geom_polygon(
          data = nn_distdf,
          aes(x = x2, y = y2, group = ID1), color = "black",
          fill = "orange", alpha = 0.3
        ) +
        geom_segment(
          aes(
            xend = x1 + 0.2 * dx,
            yend = y1 + 0.2 * dy
          ),
          linewidth = 0.5
        ) +
        labs(
          title = "Asymmetry between distance toward diagonal neighbors",
          x = "PC1", y = "PC2"
        ) +
        theme_minimal()



      output$plot_preview <- renderPlot(
        p / pa / ppi / aop / asp,
        width = 800, height = 3000
      )

      updateTabsetPanel(
        inputId = "outputs",
        session = session,
        selected = "Plot Preview"
      )
    }
  )

  observeEvent(
    input$make_dl_btn, {
      # Prepare download buttons and switch to the tab
      output$din_avg_dist <- renderUI(
        {downloadButton("dl_avg_dist", label = "Download centers_with_avgdist_and_estarea.csv")}
      )
      output$din_ind_dist <- renderUI(
        {downloadButton("dl_ind_dist", label = "Download individual_dist.csv")}
      )
      output$din_axis_angle <- renderUI(
        {downloadButton("dl_axis_angle", label = "Download axis_angle.csv")}
      )
      output$din_axis_asymmetry <- renderUI(
        {downloadButton("dl_axis_asymmetry", label = "Download asymmetry_per_axis.csv")}
      )
      updateTabsetPanel(
        inputId = "outputs",
        session = session,
        selected = "Download Links"
      )
    }
  )

  output$dl_avg_dist <- downloadHandler(
    filename = function() {
      return("centers_with_avgdist_and_estarea.csv")
    },
    content = function(file) {
      write.csv(shared_df$avg_dist, file)
    }
  )
  output$dl_ind_dist <- downloadHandler(
    filename = function() {
      return("individual_dist.csv")
    },
    content = function(file) {
      write.csv(shared_df$ind_dist, file)
    }
  )
  output$dl_axis_angle <- downloadHandler(
    filename = function() {
      return("axis_angle.csv")
    },
    content = function(file) {
      write.csv(shared_df$axis_angle, file)
    }
  )
  output$dl_axis_asymmetry <- downloadHandler(
    filename = function() {
      return("asymmetry_per_axis.csv")
    },
    content = function(file) {
      write.csv(shared_df$axis_asymmetry, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
