# Shiny app: Investigating reef data collected in Lyttelton Harbour, New Zealand


# Attach packages

library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(readxl)
library(tidyr)


cawthron_groups <- read_excel(here("data/updated_cawthron_data.xlsx"), 
                                   sheet = "functional_groups")


exclude_columns <- c("Site", "Date", "Season", "Rep")

groups_long <- cawthron_groups |>
  pivot_longer(cols = -c(exclude_columns), 
               names_to = "variable", 
               values_to = "count")
  

load("data/lyttelton_weather.RData")


# Define server logic
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
  
  # Read the data file
  updated_cawthron_data <- reactive({
    read_excel("data/updated_cawthron_data.xlsx")
  })
}

# The app infrastructure
ui <- fluidPage(
  titlePanel("Investigating Reef Data Collected in Lyttelton Harbour, New Zealand"),
  tabsetPanel(
    tabPanel("Project Overview",
             mainPanel(
               imageOutput("image_output"),
               textOutput("tab1_description")
             )
    ),
    tabPanel("The Survey Area",
             sidebarPanel(
               "The Survey Area Sidebar",
               radioButtons("radio_tab2", "Select Option:", choices = c("Control 1 (C1) Te Ara Crescent Diamond Harbour", 
                                                                        "Control 2 (C2) Pile Bay", 
                                                                        "Cass Bay (CB)", 
                                                                        "Diamond Harbour Jetty (DH)"))
             ),
             mainPanel(
               "Output for Tab 2"
             )
    ),
    tabPanel("Survey Dates",
             sidebarPanel(
               "Survey Dates Sidebar",
               radioButtons("survey_dates_tab3", "Select Survey Date:",
                            choices = c("May 2013", "November 2013", "April 2014", "May 2014", "October 2014", "February 2015", "May 2019", "October 2019"))
             ),
             mainPanel(
               "Output for Tab 3"
             )
    ),
    tabPanel("Generalized Linear Model (Poisson)",
             sidebarPanel(
               "Model Settings",
               radioButtons("model_options", "Select Model Option:",
                            choices = c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5"))
             ),
             mainPanel(
               "Output for Tab 4"
             )
    ),
    tabPanel("Animals Observed",
             sidebarPanel(
               "Animals Observed Sidebar",
               radioButtons("animals_observed", "Select Animal:",
                            choices = c("Animal 1", "Animal 2", "Animal 3", "Animal 4", "Animal 5", "Animal 6", "Animal 7", "Animal 8", "Animal 9", "Animal 10"))
             ),
             mainPanel(
               "Output for Tab 5"
             )
    )
  ),
  # Customizing the theme
  theme = shinythemes::shinytheme("slate")
)

# Run the application
shinyApp(ui = ui, server = server)

# End script
