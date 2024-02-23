# Shiny app: Investigating reef data collected in Lyttelton Harbour, New Zealand


# Attach packages

library(shiny)
library(tidyverse)
library(shinythemes)
library(here)

# The app infrastructure

ui <- fluidPage(
  titlePanel("Investigating reef data collected in Lyttelton Harbour, New Zealand"),
  sidebarLayout(
    sidebarPanel(
      "Widgets",
      radioButtons("Radio", "Select Option:", choices = c("Option 1", "Option 2", "Option 3"))
    ), # end of sidebarPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Project Overview",
                 imageOutput("image_output"),
                 textOutput("tab1_description")
        ),
        tabPanel("Tab 2", "Output for Tab 2")
      )
    ) # end of mainPanel
  ), # end of sidebarLayout
  
  # Customizing the theme
  theme = shinythemes::shinytheme("slate")
) # end of fluidPage

### Create server function

server <- function(input, output) {
  output$image_output <- renderImage({
    list(src = "www/lyttelton_harbour_twilight.jpg",
         contentType = "image/jpg",
             width = "100%", height = "auto")
  }, deleteFile = FALSE)
  
  output$tab1_description <- renderText({
    # Description text for Tab 1
    "The data for this project is sourced from quadrat surveys of
    intertidal reefs in the Lyttelton Harbour area of New
    Zealand's South Island. The focus of the surveys was invasive Undaria 
    and other aquatic organisms. This app will analyze temporal, spatial,
    and ecological patterns observed in the surveys with a specific focus
    on the impact of Undaria."
  })
}

### Combine into an app

shinyApp(ui = ui, server = server)


# End script
