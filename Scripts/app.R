#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(httpuv)
source("data_cleaning_23SPL.R")

# Shiny app----
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("2023 Seattle Public Life People Staying by Gender"),
  splitLayout(
    #    cellWidths = c("50%","50%"),
    plotlyOutput(outputId = "main_plot", height = "600px"),
    plotlyOutput(outputId = "detail_plot", height = "600px")
    # textOutput(outputId = "detail_text")
  ))

# Define server logic
server <- function(input, output) {
  # Render the main plot
  output$main_plot <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = SEA_map) +  # Plot base SEA neighborhood map
      geom_point(
        data = FM_prop_23_locid_geo,
        aes(
          x = longitude,
          y = latitude,
          color = F_prop,
          key = location_id
        ),
        shape = 20,
        size = 4
      ) +
      scale_color_gradientn(
        name = "Fem-presenting/Total",
        colours = c("mistyrose", "violetred3"),
        breaks = c(0, 0.2, 0.4, 0.6, 0.8)
      ) +
      labs(
        title = "2023 Percentage of Feminine-Presenting Individuals Staying by Observation Site",
        fill = "Observation Sites"
      )
    
    ggplotly(p, tooltip = NULL) %>%
      layout(clickmode = "event+select")  # Enable lasso selection
  })
  
  # Observe selection events
  observeEvent(event_data("plotly_selected"), {
    selected_data <- event_data("plotly_selected")
    if (is.null(selected_data)) {
      output$detail_plot <- renderPlotly(NULL)
      output$detail_text <- renderText("No points selected.")
      return(NULL)
    }
    
    # Extract the location_ids of the selected points
    selected_location_ids <- selected_data$key
    
    # Filter the original data for the selected points
    selected_points <- FM_prop_23_locid_geo[FM_prop_23_locid_geo$location_id %in% selected_location_ids, ]
    
    # Render the detail plot with selected points
    output$detail_plot <- renderPlotly({
      p_detail <- ggplot() +
        geom_sf(data = SEA_map) +  # Plot base SEA neighborhood map
        geom_point(
          data = selected_points,
          aes(
            x = longitude,
            y = latitude,
            color = F_prop,
            text = paste(
              "Block face: ", location_id,
              "<br>Percentage of Fem_presenting: ", round(F_prop, 2),
              "<br>Percentage of Masc_presenting: ", round(M_prop, 2),
              "<br>Total observation: ", total,
              "<br>Neighborhood: ", S_HOOD
            )
          ),
          shape = 20,
          size = 6
        ) +
        scale_color_gradientn(
          colours = c("mistyrose", "violetred3"),
          breaks = c(0, 0.2, 0.4, 0.6, 0.8)
        ) +
        labs(
          title = "Selected Observation Sites"
        ) +
        theme(legend.position = "none")  # Remove legend
      
      ggplotly(p_detail, tooltip = "text")
    })
    
    # Render detailed text information
    output$detail_text <- renderText({
      paste("Selected Block Faces:", paste(selected_location_ids, collapse = ", "))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
