#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(bitmapType='cairo')
# Check libraries
required <- c(
  "shiny", "shinybusy", "plotly", "data.table",
  "markdown", "mclust", "ggplot2", "dplyr", "patchwork",
  "tidyr", "RColorBrewer", "xlsx"
)

for (package in required) {
  if (!require(package, character.only = TRUE)) {
    install.packages(
      package,
      repos = "https://cloud.r-project.org"
      )
  }
}

source("module_ggdownload.R")
source("module_csvdownload.R")

library(shiny)
library(shinybusy)
library(plotly)
library(data.table)
library(markdown)
library(mclust)
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(RColorBrewer)

debug <- TRUE

# Allow upload of larger files
options(shiny.maxRequestSize = 1024^3)


theme_set(theme_bw(base_size = 18))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("RW preliminary analysis"),
    add_busy_bar(color = "#57068c", height = "12px"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          sliderInput("bin_cutoff",
                      "Probability threshold to be considered ON",
                      min = 0.1,
                      max = 1,
                      step = 0.01,
                      value = 0.5),
          uiOutput("renameLUT"),
          uiOutput("upload_main"),
          checkboxInput("use_renameLUT", "Upload a rename table"),
          checkboxInput("use_saved", "Use saved mixture model"),
          uiOutput("to_save"),
          tabsetPanel(
            id = "inputs",
            tabPanel(
              "Check Binarization",
              selectizeInput(
                "roi_bincheck",
                label = "ROI to examine",
                choices = ""
              ),
              sliderInput(
                "roi_window",
                "How many frames up/downstream to plot?",
                min = 10,
                max = 500,
                step = 10,
                value = 100
              ),
              uiOutput("roi_frame"),
              uiOutput("go_bincheck"),
              uiOutput("go_dffdist"),
              uiOutput("go_findroi"),
              uiOutput("go_rebin"),
              uiOutput("draw_map"),
              uiOutput("map_dl"),
              uiOutput("lmap_dl"),
              uiOutput("trace_dl")
            ),
            tabPanel(
              "P(Transmission)",
              checkboxInput(
                "use_all_pairs",
                "Calculate for all pairs"
              ),
              selectizeInput(
                "roi_to_trace",
                label = "ROI to examine",
                choices = ""
              ),
              sliderInput(
                "bin_num",
                label = "Split frames into ? bins",
                min = 1,
                max = 200,
                value = 20,
                step = 1
              ),
              sliderInput(
                "pt_size",
                label = "Point size (firing frequency)",
                min = 0.5,
                max = 5,
                value = 3,
                step = 0.5
              ),
              sliderInput(
                "line_width",
                label = "Max line width / point size (link strength)",
                min = 1,
                max = 3,
                value = 1.5,
                step = 0.5
              ),
              sliderInput(
                "vec_zoom",
                label = "Zoom in/out factor for vector map",
                min = 0.5,
                max = 5,
                value = 2,
                step = 0.1
              ),
              actionButton("go_trace", "Plot transmission"),
              actionButton("go_fsummary", "Show firing summary"),
              actionButton("go_lsummary", "Show link summary"),
              uiOutput("cond_dl"),
              uiOutput("fire_tbl_dl"),
              uiOutput("link_tbl_dl"),
              uiOutput("wave_tbl_dl"),
              checkboxInput(
                "merge_directionality", "Merge both directions of propagation?",
                value = TRUE
              ),
              checkboxInput(
                "use_freq", "Use frequency instead of probability for link strength",
                value = FALSE
              )
            ),
            tabPanel(
              "Rename ROIs",
              downloadButton("download_names", "Download ROI table")
            ),
            tabPanel(
              "Set color scale",
              checkboxInput(
                "use_custom_color", label = "Use custom color scale"
              ),
              uiOutput("use_fire_max"),
              uiOutput("fire_max"),
              uiOutput("use_nlink_max"),
              uiOutput("nlink_max"),
              uiOutput("use_rlink_max"),
              uiOutput("rlink_max")
            )
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "outputs",
            tabPanel(
              "Read Me",
              includeMarkdown("README.md")
            ),
            tabPanel(
              "Preview",
              tableOutput("preview"),
              uiOutput("roi_plot")
            ),
            tabPanel(
              "Binarization Check",
              plotOutput("roi_trace")
            ),
            tabPanel(
              "Transmission Probability",
              plotOutput("ntrace")
            ),
            tabPanel(
              "Firing Summary",
              plotlyOutput("fsummary_plot")
            ),
            tabPanel(
              "Link Summary",
              plotOutput("lsummary_plot")
            )
          )

        )
    )
)

server <- function(input, output, session) {
  measurement_table <- reactiveValues(df = NULL, merged = NULL)
  mnn <- data.frame()
  roi_tbl <- data.frame()
  last_slice <- numeric(1)
  n_roi <- numeric(1)
  rename_lut <- data.frame()
  bin_model <- NULL

  hideTab("inputs", "Rename ROIs")
  output$upload_main <- renderUI(
    fileInput("upload", "Upload a file"),
  )

  observeEvent(
    input$use_saved, {
      if (input$use_saved) {
        if (file.exists("saved_mclust.rds")) {
          showNotification(
            paste(
              "Loading saved object from",
              file.info("saved_mclust.rds")$ctime
            )
          )
        } else {
          showNotification("There is no saved mixture model file.")
        }
      }
    }
  )

  # Upload data
  observeEvent(
    input$upload, {
      measurement_table$df <- fread(input$upload$datapath)

      updateTabsetPanel(
        session = session,
        inputId = "outputs",
        selected = "Preview"
      )

      # Pre-process the table
      ## Assign ROI ID
      measurement_table$df$roi_id <- vapply(
        strsplit(measurement_table$df$Label, ":"), function(x) {
          x[[2]]
        },
        FUN.VALUE = character(1)
      )



      measurement_table$df <- subset(
        measurement_table$df,
        select = -c(Label, V1)
      )

      ## Generate nn map
      roi_tbl <<- measurement_table$df %>%
        filter(Slice == 1) %>%
        select(roi_id, X, Y) %>%
        as.data.frame

      if (debug) {
        print("Row names re-assignment")
      }

      ## Deal with duplicates
      if (any(duplicated(roi_tbl$roi_id))) {
        roi_tbl$roi_id <<- paste(roi_tbl$roi_id, roi_tbl$X, roi_tbl$Y, sep = "_")
        measurement_table$df[ , roi_id := paste(roi_id, X, Y, sep = "_")]

        if (debug) {
          print(head(measurement_table$df))
          print(head(roi_tbl))
        }
      }

      # Use rename LUT if exists
      if (input$use_renameLUT) {
        if (colnames(rename_lut)[1] == "roi" & colnames(rename_lut)[2] == "name") {
          if (debug) {
            print("Renaming ROIs")
          }
          # Make a dictionary
          renamedict <- rename_lut$name
          names(renamedict) <- rename_lut$roi

          roi_tbl <<- roi_tbl[roi_tbl$roi_id %in% names(renamedict), ]
          measurement_table$df <<- measurement_table$df[measurement_table$df$roi_id %in% names(renamedict), ]

          roi_tbl$roi_id <<- renamedict[roi_tbl$roi_id]
          measurement_table$df$roi_id <<- renamedict[measurement_table$df$roi_id]

        }
      }

      row.names(roi_tbl) <- roi_tbl$roi_id
      n_roi <<- length(unique(roi_tbl$roi_id))
      roi_tbl <- roi_tbl[ , -1]


      dist_mat <- roi_tbl %>%
        dist %>%
        as.matrix %>%
        as.data.frame

      dist_mat$from <- row.names(dist_mat)

      dist_mat <- dist_mat %>%
        pivot_longer(cols = -from, names_to = "to", values_to = "dist")

      dist_mat <- dist_mat[dist_mat$from != dist_mat$to, ]

      nn_10 <- dist_mat %>%
        group_by(from) %>%
        top_n(6, -dist)

      ## Expose the MNN map as a global variable
      mnn <<- as.data.frame(nn_10)
      mnn <<- mnn[
        apply(mnn, 1, function(x) {
          x[1] %in% mnn[mnn$from == x[2], "to"]
        }),
      ]

      output$preview <- renderTable(head(measurement_table$df))

      # Binarize with a Gaussian mixture model
      # TODO: Replace with proper peak calling for the trace
      # Make model global to allow saving.
      if (input$use_saved) {
        bin_model <<- readRDS("saved_mclust.rds")
        out <- predict(bin_model, measurement_table$df$IntDen)
      } else {
        bin_model <<- Mclust(measurement_table$df$IntDen, G = 2)
        out <- bin_model
      }
      on_id <- which.max(bin_model$parameters$mean)

      measurement_table$df$Pon <<- out$z[ , on_id]
      measurement_table$df$ON <<- measurement_table$df$Pon >= input$bin_cutoff
      measurement_table$on_mean <- bin_model$parameters[["mean"]][on_id]
      measurement_table$on_sd <- sqrt(bin_model$parameters[["variance"]]$sigmasq[on_id])
      measurement_table$off_mean <- bin_model$parameters[["mean"]][setdiff(c(1:2), on_id)]
      measurement_table$off_sd <- sqrt(bin_model$parameters[["variance"]]$sigmasq[setdiff(c(1:2), on_id)])

      # Remove ROIs that have always been on
      # Assuming they are background
      last_slice <<- max(measurement_table$df$Slice)

      on_tbl <- measurement_table$df %>%
        group_by(roi_id) %>%
        summarize(on_count = sum(ON)/last_slice)


      remove_omm <- on_tbl[on_tbl$on_count > 0.5, "roi_id"]

      measurement_table$df <<- filter(measurement_table$df, !roi_id %in% remove_omm)

      output$to_save <- renderUI(
        {
          actionButton("save_model", "Save current model")
        }
      )

      updateTabsetPanel(
        session,
        "inputs",
        "Check Binarization"
      )

      roi_vec <- unique(mnn$from)
      updateSelectizeInput(
        session,
        "roi_bincheck",
        choices = roi_vec
      )

      updateSelectizeInput(
        session,
        "roi_to_trace",
        choices = roi_vec
      )

      output$go_bincheck <- renderUI(
        actionButton(
          "go_bincheck",
          "Show trace"
        )
      )

      output$go_dffdist <- renderUI(
        actionButton(
          "go_dffdist",
          "Show intensity distribution"
        )
      )

      output$go_findroi <- renderUI(
        actionButton(
          "go_findroi",
          "Where's my roi?"
        )
      )

      output$draw_map <- renderUI(actionButton(
        inputId = "draw_map",
        label = "Show ROI map"
      ))
      showTab("inputs", "Rename ROIs")

      if (debug) {
        print(head(measurement_table$df))
        print(head(mnn))
        print(head(roi_tbl))
      }
    }
  )

  # Save the mixture model for later use
  observeEvent(
    input$save_model, {
      saveRDS(bin_model, "saved_mclust.rds")
    }
  )

  observeEvent(input$draw_map, {
    output$roi_plot <- renderUI(
      plotOutput("roi_map")
    )

    roi_map <- ggplot(roi_tbl,
                      aes(x = X, y = Y, label = roi_id)) +
      geom_point(size = 12, color = "black", shape = 1) +
      geom_text(aes(label = roi_id), color = "black", size = 2) +
      scale_y_reverse() +
      labs(x = "", y  = "") +
      guides(color = "none")

    updateTabsetPanel(
      session = session,
      inputId = "outputs",
      selected = "Preview"
    )

    output$roi_map <- renderPlot(
      roi_map, height = 800
    )
    output$lmap_dl <- renderUI(ggdownload_UI("lmapdl"))


    ggdownload_server(
      "lmapdl",
      filename = "roi_map_labeled",
      payload = function() {return(roi_map)}
    )

  })

  observeEvent(
    input$go_dffdist, {

      cond_pon <- function(x, on_m, on_sd, off_m, off_sd) {
        con <- pnorm(x, on_m, on_sd, lower.tail = TRUE)
        coff <- pnorm(x, off_m, off_sd, lower.tail = FALSE)
        return(con/(con + coff))
      }

     look_range <- range(measurement_table$df$IntDen)
     look_range <- seq(from = look_range[1], to = look_range[2], length.out = 101)
     pon_range <- vapply(
       look_range, cond_pon,
       on_m = measurement_table$on_mean,
       on_sd = measurement_table$on_sd,
       off_m = measurement_table$off_mean,
       off_sd = measurement_table$off_sd,
       FUN.VALUE = numeric(1)
     )
     dff_cut <- look_range[which.min(abs(input$bin_cutoff - pon_range))]

      pon_corr_hist <- measurement_table$df %>%
        ggplot(aes(x = IntDen)) +
        stat_function(
          fun = cond_pon,
          args = list(
            on_m = measurement_table$on_mean,
            on_sd = measurement_table$on_sd,
            off_m = measurement_table$off_mean,
            off_sd = measurement_table$off_sd
          ),
          color = "black"
        ) +
        geom_vline(xintercept = dff_cut) +
        annotate(
          geom = "text", x = dff_cut, y = 0.25,
          label = "Current cutoff",
          hjust = 0,
          size = 6
        ) +
        labs(
          title = "Intensity vs P(On)",
          x = "Intensity density", y = "P(On)"
        )

      if (debug) {
        print("Dff IntDen dist plot generated.")
      }

      count_max <- max(
        hist(measurement_table$df$IntDen)$counts
      )

      dff_hist <- measurement_table$df %>%
        ggplot(aes(x = IntDen)) +
        geom_histogram() +
        geom_vline(xintercept = dff_cut) +
        annotate(
          geom = "text", x = dff_cut, y = count_max,
          label = "Current cutoff",
          hjust = 0,
          size = 6
        ) +
        labs(
          title = "Intensity",
          x = "Intensity density", y = "Count of observeration-frames"
        )

      if (debug) {
        print("Dff IntDen plot generated.")
      }


      pon_hist_ymax <- max(
        hist(measurement_table$df$Pon)$counts
      )
      pon_hist <-
        measurement_table$df %>%
        ggplot(aes(x = Pon)) +
        geom_histogram() +
        geom_vline(xintercept = input$bin_cutoff) +
        annotate(
          geom = "text", x = input$bin_cutoff, y = pon_hist_ymax,
          label = "Current cutoff",
          hjust = 0,
          size = 6
        ) +
        labs(
          title = "ON Probability",
          x = "P(ON)", y = "Probability density"
        )

      if (debug) {
        print("Dff P(On) plot generated.")
      }

      outplot <- dff_hist / pon_corr_hist / pon_hist

      if (debug) {
        print("All dff plots generated.")
      }

      updateTabsetPanel(
        session,
        "outputs",
        "Binarization Check"
      )

      output$roi_trace <- renderPlot(
        outplot,
        height = 1200
      )

      output$trace_dl <- renderUI(ggdownload_UI("dffdownload"))

      ggdownload_server(
        "dffdownload",
        filename = "dff_dist",
        payload = function() {return(outplot)}
      )

    }
  )

  observeEvent(
    input$go_bincheck, {
      roi_check_bin <- input$roi_bincheck

      on_frames <- measurement_table$df %>%
        group_by(roi_id) %>%
        filter(roi_id == roi_check_bin) %>%
        filter(ON == TRUE) %>%
        arrange(roi_id, Slice) %>%
        as.data.frame

      if (nrow(on_frames) > 1) {

        # Find gaps between ON frames by dropping consecutive frames
        on_frames$dur <- -1
        on_frames[-1, "dur"] <- diff(on_frames$Slice)

        if (any(on_frames$dur > 1)) {
          on_frames <- subset(on_frames, dur > 1) %>%
            arrange(dur)

          # Show frame selection
          output$roi_frame <- renderUI(
            selectInput(
              inputId = "roi_frame",
              label = "Which frame to show?",
              choices = unique(on_frames$Slice)
            )
          )

          window <- ifelse(
            is.null(input$roi_frame),
            on_frames$Slice[1],
            as.integer(input$roi_frame)
          )


          # Plot modeled probability and binarization result
          pplot <- measurement_table$df %>%
            filter(roi_id == input$roi_bincheck) %>%
            filter(
              Slice > window - as.integer(input$roi_window), Slice < window + as.integer(input$roi_window)
            ) %>%
            arrange(roi_id, Slice) %>%
            ggplot(aes(x = Slice, y = Pon)) + geom_line() +
            geom_point(aes(y = as.integer(ON))) +
            labs(
              y = "Probability ON",
              x = "Frame",
              title = "Binarization",
              subtitle = "Point: ON or OFF\nLine: Inferred probability"
            )

          # Plot integrated intensity
          iplot <- measurement_table$df %>%
            filter(roi_id == input$roi_bincheck) %>%
            filter(
              Slice > window - input$roi_window, Slice < window + input$roi_window
            ) %>%
            arrange(roi_id, Slice) %>%
            ggplot(aes(x = Slice, y = IntDen)) +
            geom_line() +
            labs(
              y = expression(Integrated ~ Delta ~ F/F[0] ~ per ~ ROI),
              x = "Frame",
              title = expression(Integrated ~ Delta ~ F/F[0]),
            )

          output$roi_trace <- renderPlot(
            pplot / iplot,
            height = 1000
          )
          updateTabsetPanel(
            session,
            "outputs",
            "Binarization Check"
          )
          output$go_rebin <- renderUI(
            actionButton("go_rebin", "Binarize with new cutoff")
          )

          output$map_dl <- renderUI(ggdownload_UI("tdownload"))

          ggdownload_server(
            "tdownload",
            filename = paste0(
              "trace_", input$roi_bincheck, "_", window, "_span_",
              input$roi_window
            ),
            payload = function() {return(pplot / iplot)}
          )
        } else {
          paste(
            "ROI", input$roi_bincheck, "is always ON under this cutoff.",
            "Please try another one."
          )
        }

      } else {
        showNotification(
          paste(
            "ROI", input$roi_bincheck, "is never ON under this cutoff.",
            "Please try another one."
          )
        )
      }
    }
  )

  observeEvent(
    input$go_findroi, {
      output$roi_plot <- renderUI(
        plotlyOutput("roi_find")
      )

      roi_plot <- roi_tbl %>%
        mutate(highlight = roi_id == input$roi_bincheck)

      roi_map <- ggplot(roi_plot,
             aes(x = X, y = Y, label = roi_id, color = highlight)) +
        geom_point(size = 5) +
        geom_text(data = filter(roi_plot, highlight == TRUE),
          aes(label = roi_id), color = "black", size = 6) +
        labs(x = "", y  = "") +
        scale_color_manual(values = c("grey30", "green")) +
        scale_y_reverse() +
        guides(color = "none")

      updateTabsetPanel(
        session = session,
        inputId = "outputs",
        selected = "Preview"
      )

      output$roi_plot <- renderUI(
        plotlyOutput("roi_find")
      )
      output$roi_find <- renderPlotly(
        ggplotly(roi_map, tooltip = "roi_id", height = 1000)
      )

      output$map_dl <- renderUI(ggdownload_UI("mdownload"))

      ggdownload_server(
        "mdownload",
        filename = paste0("roi_map_", input$roi_bincheck),
        payload = function() {return(roi_map)}
      )

    }
  )

  observeEvent(
    input$go_rebin, {
      measurement_table$df$ON <<- measurement_table$df$Pon >= input$bin_cutoff
      showNotification("Cut-off updated.")
    }
  )

  observeEvent(
    input$use_all_pairs,
    {
      print(n_roi)
      if (input$use_all_pairs & n_roi >= 30) {
        showModal(modalDialog(
          title = "Warning: Using all ROI pairs",
          paste(
            "The current data contains",
            n_roi,
            "distinct ROIs, and using all ROI pairs will result in",
            "a tremendous amount of calculation and might crash the server."
          )
        ))
      }
    }
  )

  observeEvent(
    input$go_trace,
    {
      roioi <- input$roi_to_trace

      if (
        nrow(filter(measurement_table$df, roi_id == roioi, ON == TRUE)) > 0
      ) {
        # Get the potential triggered frames per ROI of interest
        # (The next frame when ROI of interest is ON)
        mnn1_pttri <- measurement_table$df %>%
          filter(roi_id == roioi, ON == TRUE) %>%
          pull(Slice) + 1

        tplot <- measurement_table$df %>%
          # Bin the frames
          mutate(bin = ntile(Slice, n = input$bin_num)) %>%
          # For every potential trigger frame that belongs to a neighbor
          filter(
            roi_id %in% subset(mnn, from == roioi)$to,
            Slice %in% mnn1_pttri
          ) %>%
          group_by(roi_id, bin) %>%
          summarize(on_freq = sum(ON)/n()) %>%
          ggplot(aes(x = bin, y = on_freq, color = roi_id)) +
          geom_smooth(se = FALSE) +
          scale_y_continuous(labels = scales::percent) +
          scale_color_brewer(palette = "Dark2") +
          labs(
            x = paste0(
              "Bin # (",
              round(last_slice/input$bin_num, digit = 0),
              " frames per bin)"
            ),
            y = "Probability of transmission (%)",
            color = "Ommatidium ID"
          ) +
          geom_point()

        output$ntrace <- renderPlot(tplot)

        output$cond_dl <- renderUI(ggdownload_UI("ptdownload"))


        ggdownload_server(
          "ptdownload",
          filename = paste0("roi_nn_", roioi, "_", input$bin_num),
          payload = function() {return(tplot)}
        )

        updateTabsetPanel(
          session,
          "outputs",
          "Transmission Probability"
        )
      } else {
        showNotification(
          paste(
            "ROI", input$roi_bincheck, "is never ON.",
            "Please try another one."
          )
        )
      }
    }
  )

  observeEvent(input$go_fsummary, {
    on_count <- measurement_table$df %>%
      # Bin the frames
      mutate(bin = ntile(Slice, n = input$bin_num)) %>%
      group_by(roi_id, bin) %>%
      summarize(ON_count = sum(ON))


    on_count <- merge(on_count, roi_tbl, by = "roi_id")

    use_fire_max <- ifelse(!is.null(input$use_fire_max), input$use_fire_max, FALSE)
    scale_max <- ifelse(
      input$use_custom_color & use_fire_max,
      input$fire_culimit,
      quantile(on_count$ON_count, probs = 0.8)
    )

    if (debug) {
      print(head(on_count))
      print(scale_max)
    }

    on_plot <- on_count %>%
      ggplot(aes(x = X, y = Y, color = ON_count)) +
      geom_point(size = input$pt_size) +
      scale_color_viridis_c(
        option = "C", limits = c(0, scale_max),
        oob = scales::squish
      ) +
      labs(
        x = "",
        y = "",
        title = "Firing frequency by bin"
      ) +
      scale_y_reverse() +
      facet_wrap(~bin, ncol = 4)

    output$fsummary_plot <- renderPlotly(
      ggplotly(
        on_plot,
        height = max(input$bin_num * 70, 500),
        tooltip = "ON_count"
      )
    )

    output$cond_dl <- renderUI(ggdownload_UI("fdownload"))
    output$fire_tbl_dl <- renderUI(
      csvdownload_UI("ftbldownload", "Download CSV (Firing summary)")
    )
    csvdownload_server(
      "ftbldownload",
      filename = "firing_frequency_per_bin_per_roi.csv",
      payload = function() {return(on_count)}
    )

    updateTabsetPanel(
      session,
      "outputs",
      "Firing Summary"
    )

    ggdownload_server(
      "fdownload",
      filename = paste0("firing_freq_", input$bin_num),
      payload = function() {return(on_plot)}
    )
    }
  )

  observeEvent(input$go_lsummary, {
    binned_data <- measurement_table$df %>%
      # Bin the frames
      mutate(bin = ntile(Slice, n = input$bin_num))

    rois <- unique(mnn$from)
    names(rois) <- unique(mnn$from)
    mnnl <- lapply(
      rois, function(x) {
        return(subset(mnn, from == x)$to)
      }
    )


    binned_freq <- lapply(rois, function(roioi) {
      mnn1_pttri <- binned_data %>%
        filter(roi_id == roioi, ON == TRUE) %>%
        pull(Slice) + 1

      if (input$use_all_pairs) {
        out <- binned_data %>%
          # For every potential trigger frame that belongs to a neighbor
          filter(
            Slice %in% mnn1_pttri
          )
      } else {
        out <- binned_data %>%
          # For every potential trigger frame that belongs to a neighbor
          filter(
            roi_id %in% mnnl[[roioi]],
            Slice %in% mnn1_pttri
          )
      }

      out <- out %>%
        group_by(roi_id, bin) %>%
        summarize(
          on_prob = sum(ON)/n(), on_freq = sum(ON), .groups = "drop"
        ) %>%
        mutate(from = roioi) %>%
        rename(to = roi_id)
      return(out)
    })

    # Make data.frame from list
    binned_freq <- do.call(rbind.data.frame, binned_freq)

    # Merge bidirectional links
    if (input$merge_directionality) {
      binned_freq$link_uid <- apply(
        binned_freq, 1, function(x) {
          partners <- c(x["from"], x["to"])
          uid <- paste(
            sort(partners),
            collapse = "@"
          )
          return(uid)
        }
      )
      binned_freq <- binned_freq %>%
        group_by(link_uid, bin) %>%
        summarize(on_prob = mean(on_prob),
                  on_freq = mean(on_freq),
                  .groups = "drop") %>%
        separate_wider_delim(link_uid, "@", names = c("from", "to"))
    }

    # Get MNN table with coordinates
    binned_freq <- merge(binned_freq, roi_tbl, by.x = "from", by.y = "roi_id")
    colnames(binned_freq)[colnames(binned_freq) == "X"] <- "from_X"
    colnames(binned_freq)[colnames(binned_freq) == "Y"] <- "from_Y"
    binned_freq <- merge(binned_freq, roi_tbl, by.x = "to", by.y = "roi_id")
    colnames(binned_freq)[colnames(binned_freq) == "X"] <- "to_X"
    colnames(binned_freq)[colnames(binned_freq) == "Y"] <- "to_Y"


    if (input$use_freq) {
      link_col <- "on_freq"
      ptype <- "frequency"
    } else {
      link_col <- "on_prob"
      ptype <- "probability"
    }

    vec_sum <- binned_freq %>%
      group_by(from, bin, from_X, from_Y) %>%
      mutate(
        vec_x = (to_X - from_X) * sqrt(.data[[link_col]]),
        vec_y = (to_Y - from_Y) * sqrt(.data[[link_col]])
      ) %>%
      summarize(sum_x = sum(vec_x), sum_y = sum(vec_y)) %>%
      filter(sum_x > 0 | sum_y > 0)


    if (input$use_freq) {
      vec_len <- sqrt(vec_sum$sum_x ^ 2 + vec_sum$sum_y ^ 2)

      scale_factor <- log(vec_len)/vec_len * input$vec_zoom
      vec_sum$sum_x <- vec_sum$sum_x * scale_factor
      vec_sum$sum_y <- vec_sum$sum_y * scale_factor

    } else {
      # The default for this option is 2, but for P(Trans), 1 seems better.
      # To make it also adjustable, I added the correction factor of 2.
      vec_sum$sum_x <- vec_sum$sum_x * input$vec_zoom / 2
      vec_sum$sum_y <- vec_sum$sum_y * input$vec_zoom / 2
    }


    vec_sum <- mutate(vec_sum, to_X = from_X + sum_x, to_Y = from_Y + sum_y)

    arrow_ends <- ifelse(input$merge_directionality, "both", "last")
    vfield_plot <- vec_sum %>%
      ggplot(aes(x = from_X, y = from_Y)) +
      geom_segment(
        aes(xend = to_X, yend = to_Y),
        arrow = arrow(length = unit(0.1,"cm"), ends = arrow_ends)
        ) +
      labs(
        title = paste("Transmission", ptype, "per bin"),
        subtitle = "Per ROI as vector sum",
        x = "",
        y = ""
      ) +
      scale_y_reverse() +
      facet_wrap(~bin, ncol = 4)


    linkalpha <- ifelse(input$merge_directionality, 1, 0.5)
    linkarrwidth <- ifelse(input$merge_directionality, 0, 0.2)
    scale_max <- ifelse(
      input$use_freq,
      quantile(binned_freq[[link_col]], 0.95),
      0.5
    )

    use_nlink_max <- ifelse(!is.null(input$use_nlink_max), input$use_nlink_max, 0)
    scale_max <- ifelse(
      input$use_custom_color & use_nlink_max,
      input$nlink_culimit, scale_max
    )

    link_plot <- binned_freq %>%
      ggplot(aes(linewidth = .data[[link_col]], color = .data[[link_col]])) +
      geom_segment(
        aes(x = from_X, y = from_Y, xend = to_X, yend = to_Y),
        arrow = arrow(length = unit(linkarrwidth, "cm")),
        alpha = linkalpha
        ) +
      scale_color_viridis_c(
        option = "plasma",
        limits = c(0, scale_max),
        oob = scales::squish
      ) +
      scale_linewidth_continuous(
        range = c(0.2, input$line_width),
        limits = c(0.01, scale_max),
      ) +
      labs(
        title = paste("Transmission", ptype, "per bin"),
        subtitle = "Per neighbor",
        x = "",
        y = "",
        color = ifelse(input$use_freq, "# of Transmission", "P(Transmission)")
      ) +
      scale_y_reverse() +
      guides(linewidth = "none") +
      facet_wrap(~bin, ncol = 4)

    lsum <- binned_freq %>%
      group_by(from, bin, from_X, from_Y) %>%
      mutate(link_total = sum(.data[[link_col]]))

    print(str(input$rlink_culimit))
    use_rlink_max <- ifelse(!is.null(input$use_rlink_max), input$use_rlink_max, 0)
    cap_percentile <- ifelse(
      input$use_custom_color & use_rlink_max,
      input$rlink_culimit,
      quantile(lsum$link_total, 0.9)
    )

    lsum_plot <- lsum %>%
      ggplot(aes(x = from_X, y = from_Y)) +
      geom_point(aes(color = link_total), size = input$line_width * 2.5) +
      scale_color_viridis_c(
        option = "C",
        limits = c(0, cap_percentile),
        oob = scales::squish
      ) +
      scale_y_reverse() +
      labs(
        title = paste("Transmission", ptype, "per bin"),
        subtitle = "Per ROI as link strength sum",
        x = "",
        y = "",
        color = ifelse(input$use_freq, "# of Transmission", "P(Transmission)")
      ) +
      facet_wrap(~bin, ncol = 4)

    # ##### Wave Anno (working)
    # # Retrieve all neighbors that could transmit signal to an ROI
    rois <- unique(mnn$to)
    names(rois) <- unique(mnn$to)
    mnn_from <- lapply(
      rois, function(x) {
        return(subset(mnn, to == x)$from)
      }
    )

    ## Add 3 more fields:
    ## 1. wave_id: A unique identifier for each wave
    ## 2. collision: If an ROI is in between two waves
    ## 3. de_novo: If the ROI initiate a new wave
    binned_data$wave_id <- "0"
    binned_data$collision <- FALSE
    binned_data$de_novo <- FALSE

    per_slice_list <- split(binned_data, binned_data$Slice)
    wave_count <- 0

    for (slice_idx in seq_along(per_slice_list)) {
      current <- per_slice_list[[slice_idx]]

      if (!any(current$ON)) {
        next
      }

      on_df <- current[current$ON, ]

      potential_triggers <- lapply(
        on_df$roi_id, function(roi) {
          ## For the first frame, there is no potential trigger.
          if (slice_idx == 1) {
            return(NULL)
          }

          trigger_name <- mnnl[[roi]]
          prev <- per_slice_list[[slice_idx - 1]]

          self_on <- prev$ON[prev$roi_id == roi]
          self_on <- ifelse(length(self_on) == 0, FALSE, self_on)
          prev_trigger_on <- prev$roi_id %in% trigger_name & prev$ON
          if (self_on) {
            return(prev[prev$roi_id == roi, "wave_id"])
          }
          if (any(prev_trigger_on)) {
            out <- prev$wave_id[prev_trigger_on]
          } else {
            return(NULL)
          }
          return(unique(out))
        }
      )

      for (trigger_idx in seq_along(potential_triggers)) {
        current_trigger <- potential_triggers[[trigger_idx]]

        # When there is no trigger
        if (is.null(current_trigger)) {
          wave_count <- wave_count + 1
          on_df[trigger_idx, "wave_id"] <- as.character(wave_count)
          on_df[trigger_idx, "collision"] <- FALSE
          on_df[trigger_idx, "de_novo"] <- TRUE
        }

        # When there is only one trigger wave found
        if (length(current_trigger) == 1) {
          on_df[trigger_idx, "wave_id"] <- current_trigger
          on_df[trigger_idx, "collision"] <- FALSE
          on_df[trigger_idx, "de_novo"] <- FALSE
        }

        # When there are multiple triggers from different waves
        if (length(current_trigger) > 1) {
          # Deal with a collision trigger
          current_trigger <- unlist(
            strsplit(current_trigger, "_")
          )
          on_df[trigger_idx, "wave_id"] <- paste(unique(current_trigger), collapse = "_")
          on_df[trigger_idx, "collision"] <- TRUE
          on_df[trigger_idx, "de_novo"] <- FALSE
        }
      }

      per_slice_list[[slice_idx]] <- on_df
    }

    on_concat_df <- do.call(rbind.data.frame, per_slice_list)

    # ws_plot <- on_concat_df %>%
    #   ggplot(aes(color = wave_id, shape = collision, fill = de_novo)) +
    #   geom_point(
    #     aes(x = X, y = Y), size = input$pt_size, alpha = 0.5
    #   ) +
    #   labs(
    #     title = "Wave summary",
    #     subtitle = "Each color is a wave",
    #     x = "",
    #     y = ""
    #   ) +
    #   guides(fill = "none", shape = "none", color = "none") +
    #   scale_y_reverse() +
    #   facet_wrap(~bin, ncol = 4)
    ###########################################################################



    outplot <- link_plot / lsum_plot / vfield_plot# / ws_plot

    output$lsummary_plot <- renderPlot(
      outplot,
      height = max(input$bin_num * 250 + 700, 1400)
    )

    output$cond_dl <- renderUI(ggdownload_UI("ldownload"))
    output$link_tbl_dl <- renderUI(csvdownload_UI("ltbldownload", "Download CSV (Link)"))
    csvdownload_server(
      "ltbldownload",
      filename = "transmission_per_bin_per_roi.csv",
      payload = function() {return(lsum)}
    )

    output$wave_tbl_dl <- renderUI(csvdownload_UI("wtbldownload", "Download CSV (Wave)"))
    csvdownload_server(
      "wtbldownload",
      filename = "waveanno.csv",
      payload = function() {return(on_concat_df)}
    )

    ggdownload_server(
      "ldownload",
      filename = paste0("link_str_", input$bin_num),
      payload = function() {return(outplot)}
    )

    updateTabsetPanel(
      session,
      "outputs",
      "Link Summary"
    )
  }
  )

  output$download_names <- downloadHandler(
    filename = "roi_naming.csv",
    content = function(file) {
      write.csv(
        data.frame(
          roi = unique(mnn$from),
          name = ""
        ),
        file
      )
    }
  )

  observeEvent(
    input$use_renameLUT, {
      if (input$use_renameLUT) {
        output$renameLUT <- renderUI(
          fileInput("upload_nameLUT", "Upload ROI naming table")
        )
        output$upload_main <- renderUI(NULL)
      } else {
        output$renameLUT <- renderUI(NULL)
        output$upload_main <- renderUI(
          fileInput("upload", "Upload a file"),
        )
      }
    }
  )

  observeEvent(
    input$upload_nameLUT, {
      temp_lut <- read.csv(input$upload_nameLUT$datapath, row.names = 1)
      print(temp_lut)

      # Deal with possible duplicated labels
      dup_bool <- duplicated(temp_lut$name)
      if (any(dup_bool)) {
        dup_label <- unique(temp_lut$name[dup_bool])
        for (lbl in dup_label) {
          to_seq <- temp_lut$name[temp_lut$name == lbl]
          to_seq <- paste(to_seq, seq_along(to_seq), sep = "_")
          temp_lut$name[temp_lut$name == lbl] <- to_seq
        }
      }
      rename_lut <<- temp_lut
      if (debug) {
        print(head(rename_lut))
      }
      output$upload_main <- renderUI(
        fileInput("upload", "Upload a file"),
      )
    }
  )

  observeEvent(
    input$use_custom_color, {
      if (input$use_custom_color) {
        output$use_fire_max <- renderUI(
          {checkboxInput(
            "use_fire_max", "Use custom scale (Firing)", value = TRUE
          )}
        )
        output$use_nlink_max <- renderUI(
          {checkboxInput(
            "use_nlink_max", "Use custom scale (Link per neighbor)", value = TRUE
          )}
        )

        output$use_rlink_max <- renderUI(
          {checkboxInput(
            "use_rlink_max", "Use custom scale (Link per ROI)", value = TRUE
          )}
        )
      } else {
        output$use_fire_max <- NULL
        output$use_nlink_max <- NULL
        output$use_rlink_max <- NULL
      }
    }
  )
  observeEvent(
    input$use_fire_max, {
      if (input$use_fire_max == 1) {
        output$fire_max <- renderUI(
          {
            numericInput(
              "fire_culimit", "Max value for color scale (Firing)",
              value = 0, min = 0
            )
          }
        )
      } else {
        output$fire_max <- NULL
      }
    }
  )
  observeEvent(
    input$use_nlink_max, {
      if (input$use_nlink_max == 1) {
        output$nlink_max <- renderUI(
          {
            numericInput(
              "nlink_culimit", "Max value for color scale (Link per neighbor)",
              value = 0, min = 0
            )
          }
        )
      } else {
        output$nlink_max <- NULL
      }
    }
  )
  observeEvent(
    input$use_rlink_max, {
      if (input$use_rlink_max == 1) {
        output$rlink_max <- renderUI(
          {
            numericInput(
              "rlink_culimit", "Max value for color scale (Link per ROI)",
              value = 0, min = 0
            )
          }
        )
      } else {
        output$rlink_max <- NULL
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
