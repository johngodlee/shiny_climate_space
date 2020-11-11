# Visualise climate space of SEOSAW plots
# John Godlee (johngodlee@gmail.com)
# 2020-06-02

# Preamble ----

# Packages
library(raster)
library(ggplot2)
library(Cairo)

# Import data
plots <- readRDS("data/plots.rds")
africa <- readRDS("data/africa.rds")
rast <- readRDS("data/rast.rds")
val <- readRDS("data/val.rds")

raster_choices <- list(
  "MAP" = "map",
  "MAT" = "mat",
  "Fire frequency" = "fire_count",
  "Herbivore biomass" = "total_herbiv",
  "Days of frost" = "frost_days",
  "Travel time to city" = "travel_time",
  "Human population" = "pop_density")

bg_col <- "#ce552c"
fg_col <- "#33a1b1"

map_plot <- ggplot() +  
      geom_sf(data = africa)

# UI ----
ui <- fluidPage(
  fluidRow(
    column(3,
      selectInput("xvar", 
        label = "Choose X variable",
        choices = raster_choices,
        selected = "mat"),
     checkboxInput("xlog",
       label = "Log-transform",
       value = FALSE)
      ),
    column(3,
      selectInput("yvar", 
        label = "Choose Y variable",
        choices = raster_choices,
        selected = "map"),
     checkboxInput("ylog",
       label = "Log-transform",
       value = FALSE)
    ),
    column(3,
      checkboxInput("pdisp",
        label = "Display points",
        value = TRUE)
    )
  ),
  fluidRow(
    column(6,
      plotOutput("plot1", brush = "brush")
    ),
    column(6,
      plotOutput("plot2", height = "580")
    )
  )
)

# Server ----
server <- function(input, output) {

  # Options
  options(shiny.usecairo = TRUE)
  
  # Subset raster by selected area for map 
  rasterMapInput <- reactive({
    rast_sel <- stack(rast[[input$xvar]], rast[[input$yvar]])
    if (is.null(input$brush)) {
      rast_sub <- rast_sel
      rast_spdf <- rasterToPoints(rast_sub[[1]], spatial = TRUE)
      rast_df <- as.data.frame(rast_spdf)
      colnames(rast_df) <- c("value", "x", "y")
      rast_df
    } else {
      if (input$xlog) {
        xmin <- exp(input$brush$xmin)
        xmax <- exp(input$brush$xmax)
      } else {
        xmin <- input$brush$xmin
        xmax <- input$brush$xmax
      }
      if (input$ylog) {
        ymin <- exp(input$brush$ymin)
        ymax <- exp(input$brush$ymax)
      } else {
        ymin <- input$brush$ymin
        ymax <- input$brush$ymax
      }
      rast_sub <- rast_sel
      rast_sub[[1]][
        rast_sub[[1]] < xmin |
        rast_sub[[1]] > xmax |
        rast_sub[[2]] < ymin | 
        rast_sub[[2]] > ymax] <- NA
      rast_spdf <- rasterToPoints(rast_sub[[1]], spatial = TRUE)
      rast_df <- as.data.frame(rast_spdf)
      colnames(rast_df) <- c("value", "x", "y")
      rast_df
    }
  })

  # Subset plots by selected area and layers
  plotsMapInput <- reactive({
    if (input$pdisp) {
      plots_layers <- plots[, c("lon", "lat", input$xvar, input$yvar)]
    } else {
      plots_layers <- data.frame(lon = NA_real_, lat = NA_real_, 
        xvar = NA_real_, yvar = NA_real_)
    }

    if (is.null(input$brush)) {
      plots_sub <- plots_layers 
      plots_sub
    } else {
      if (input$xlog) {
        xmin <- exp(input$brush$xmin)
        xmax <- exp(input$brush$xmax)
      } else {
        xmin <- input$brush$xmin
        xmax <- input$brush$xmax
      }
      if (input$ylog) {
        ymin <- exp(input$brush$ymin)
        ymax <- exp(input$brush$ymax)
      } else {
        ymin <- input$brush$ymin
        ymax <- input$brush$ymax
      }
      plots_sub <- plots_layers[
        plots_layers[,3] > xmin &
        plots_layers[,3] < xmax &
        plots_layers[,4] > ymin &
        plots_layers[,4] < ymax,] 
      plots_sub
    }
  })

  # Extract values from selected raster layers
  valInput <- reactive({
    val_input <- as.data.frame(val[,c(input$xvar, input$yvar)])
    if (input$xlog) {
      val_input[,1] <- log(val_input[,1])
    } 
    if (input$ylog) {
      val_input[,2] <- log(val_input[,2])
    } 
    val_input
  })

  plotsInput <- reactive({
    if (input$pdisp) {
      plots_input <- as.data.frame(plots[,c(input$xvar, input$yvar)])
    } else {
      plots_input <- data.frame(xvar = NA_real_, yvar = NA_real_)
    }
    if (input$xlog) {
      plots_input[,1] <- log(plots_input[,1])
    } 
    if (input$ylog) {
      plots_input[,2] <- log(plots_input[,2])
    } 
    plots_input
  })

  output$plot1 <- renderPlot(
    ggplot() + 
      geom_bin2d(data = valInput(), 
        mapping = aes_string(x = names(valInput())[1], y = names(valInput())[2]),
          colour = bg_col, fill = bg_col, bins = 100) + 
      geom_point(data = plotsInput(), 
        mapping = aes_string(x = names(plotsInput())[1], y = names(plotsInput())[2]), 
        colour = "black", fill = fg_col, shape = 21) + 
      theme_classic() + 
#      labs(y = expression("MAT" ~ (degree*C)), 
#        x = expression("MAP" ~ (mm ~ y^-1))) + 
      theme(legend.position = "none")
  )
  output$plot2 <- renderPlot(
    map_plot + 
      geom_tile(data = rasterMapInput(), aes(x = x, y = y), fill = bg_col) + 
      geom_point(data = plotsMapInput(), 
        aes(x = lon, y = lat),
        colour = "black", fill = fg_col, shape = 21) + 
      theme_classic() + 
      theme(legend.position = "none") + 
      labs(x = "", y = "")
  )
}

# Run
shinyApp(ui, server)
