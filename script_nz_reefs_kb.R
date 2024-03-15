# Shiny app: Investigating reef data collected in Lyttelton Harbour, New Zealand


# Attach packages

library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(readxl)
library(tidyr)
library(leaflet)
library(sf)
library(dplyr)
library(knitr)

### Original dataset from excel
cawthron_groups <- read_excel(here("data/updated_cawthron_data.xlsx"), 
                                   sheet = "functional_groups")

### Tidying names
new_names <- c("site", "date", "season", "rep", "undaria_percent", 
               "undaria_adults", "undaria_juveniles",  "undaria_recruits", 
               "sum_undaria_AdultsJuveniles", "percent_fucoids_kelps", "percent_solitary_bivalves", 
               "percent_solitary_anemones", "percent_barnacles_worms", "percent_colonial_FilterFeeders", 
               "percent_reds", "percent_greens", "percent_browns", "percent_algal_crusts", 
               "percent_red_blades", "percent_green_blades", "percent_molluscan_herbivores", 
               "crustacean_herbivore_count", "percent_molluscan_predators", "crustacean_predator_count", 
               "echinoderm_predator_count", "percent_coralline_turf", "percent_paint", "percent_bare_space")
colnames(cawthron_groups) <- new_names


### Prep to pivot_longer
exclude_columns <- c("site", "date", "season", "rep")

### pivot_longer
groups_long <- cawthron_groups |>
  pivot_longer(cols = -c(exclude_columns), 
               names_to = "variable", 
               values_to = "count_percent")

### Add a column to join groups_long with weather data
groups_long <- groups_long |>
  mutate(month_year = case_when(
    date == "2013" & season == "Autumn" ~ "may_2013",
    date == "2013" & season == "Spring" ~ "november_2013",
    date == "2014" & season == "Autumn" ~ "may_2014",
    date == "2014" & season == "Spring" ~ "october_2014",
    date == "2015" & season == "Autumn" ~ "february_2015",
    date == "2019" & season == "Autumn" ~ "may_2019",
    date == "2019" & season == "Spring" ~ "october_2019",
    TRUE ~ NA_character_  # For other cases, set it as NA
  ))
  

### Load weather data
load("data/lyttelton_weather.RData")

### Add a column to join with groups_long
lyttelton_weather <- lyttelton_weather |>
  mutate(month_year = case_when(
    time == "2013-05-16T12:00:00Z" ~ "may_2013",
    time == "2013-11-16T12:00:00Z" ~ "november_2013",
    time == "2014-05-16T12:00:00Z" ~ "may_2014",
    time == "2014-10-16T12:00:00Z" ~ "october_2014",
    time == "2015-02-16T12:00:00Z" ~ "february_2015",
    time == "2019-05-16T12:00:00Z" ~ "may_2019",
    time == "2019-10-16T12:00:00Z" ~ "october_2019",
    TRUE ~ NA_character_  # For other cases, set it as NA
  )) |>
  filter(!is.na(month_year))

### Join groups_long and lyttelton_weather
groups_weather <- left_join(groups_long, lyttelton_weather, by = "month_year")


### Add a column to join cawthron_groups with weather data
cawthron_groups <- cawthron_groups |>
  mutate(month_year = case_when(
    date == "2013" & season == "Autumn" ~ "may_2013",
    date == "2013" & season == "Spring" ~ "november_2013",
    date == "2014" & season == "Autumn" ~ "may_2014",
    date == "2014" & season == "Spring" ~ "october_2014",
    date == "2015" & season == "Autumn" ~ "february_2015",
    date == "2019" & season == "Autumn" ~ "may_2019",
    date == "2019" & season == "Spring" ~ "october_2019",
    TRUE ~ NA_character_  # For other cases, set it as NA
  ))

### Join cawthron_groups and lyttelton_weather
original_groups_weather <- left_join(cawthron_groups, lyttelton_weather, by = "month_year")


### Create a place to store the survey location coordinates
survey_locations <- data.frame(
  site = c("DH", "C2", "CB", "C1"),
  longitude = c(172.736142, 172.761634, 172.695458, 172.730735),
  latitude = c(-43.622509, -43.620521, -43.608217, -43.625600))

### Create an sf object of survey location coordinates from the data frame
survey_locations_sf <- st_as_sf(survey_locations, coords = c("longitude", "latitude"))



# The app infrastructure
ui <- fluidPage(
  
  ### Tab 1
  titlePanel("Investigating Reef Data Collected in Lyttelton Harbour, New Zealand"),
  tabsetPanel(
    tabPanel("Project Overview",
             mainPanel(
               imageOutput("image_output"),
               textOutput("tab1_description")
             )
    ),
    
    ### Tab 2
    tabPanel("The Survey Area",
             sidebarPanel(
               radioButtons("radio_tab2", "Survey Sites:", 
                            choices = c("Control 1 Te Ara Crescent Diamond Harbour" = "C1", 
                                        "Control 2 Pile Bay" = "C2", 
                                        "Cass Bay" = "CB", 
                                        "Diamond Harbour Jetty" = "DH"))
             ),
             mainPanel(
               leafletOutput("survey_map"),
               tableOutput("map_table")  
             )
    ),
    
    ### Tab 3
    tabPanel("Survey Dates",
             sidebarPanel(
               "Survey Dates Sidebar",
               selectInput("survey_dates_tab3", "Select Survey Date:",
                           choices = c(
                                       "November 2013" = "november_2013", 
                                       "May 2014" = "may_2014", 
                                       "October 2014" = "october_2014", 
                                       "February 2015" = "february_2015", 
                                       "May 2019" = "may_2019", 
                                       "October 2019" = "october_2019"))
             ),
             mainPanel(
               tableOutput("date_table")
             )
    ),
    
    ### Tab 4
    tabPanel("Linear Regressions",
             sidebarPanel(
               "Model Settings",
               selectInput("model_options", "Select Model Option:",
                           choices = c("Fucoids and Kelps" = "percent_fucoids_kelps", 
                                       "Coralline Turfs" = "percent_coralline_turf", 
                                       "Paints" = "percent_paint"))
             ),
             mainPanel(
               plotOutput(outputId = "undaria_plot"),
               tableOutput(outputId = "lm_table")
             )
    ),
    
    ### Tab 5
    tabPanel("Animals Observed",
             sidebarPanel(
               "Animals Observed Sidebar",
               radioButtons("animals_observed", "Select Animal:",
                            choices = c("Crustacean herbivores", 
                                        "Crustacean predators", 
                                        "Echinoderm predators", 
                                        "Solitary bivalves",
                                        "Solitary anemones",
                                        "Barnacles and worms",
                                        "Colonial filter feeders",
                                        "Molluscan herbivores",
                                        "Molluscan predators"))
             ),
             mainPanel(
               tableOutput("animal_table")
             )
    )
  ),
  
  # Customizing the theme
  theme = shinythemes::shinytheme("slate")
)


# Create server function

server <- function(input, output) {
  ### Write out desired output
  # Tab 1
  ### Tab 1 image
  output$image_output <- renderImage({
    list(src = "www/lyttelton_harbour_twilight.jpg",
         contentType = "image/jpg",
         width = "100%", height = "auto")
  }, deleteFile = FALSE)
  
  ### Tab 1 description
  output$tab1_description <- renderText({
    "The data for this project is sourced from quadrat surveys of
    intertidal reefs in the Lyttelton Harbour area of New
    Zealand's South Island. The focus of the surveys was invasive Undaria 
    and other aquatic organisms. This app will analyze temporal, spatial,
    and ecological patterns observed in the surveys with a specific focus
    on the impact of invasive Undaria."
  })
  
  # Tab 2
  ### Reactive value to store selected point
  # selected_point <- reactiveVal(NULL)
  
  ### Observer for leaflet map click events
  # observe({
    # click <- input$survey_map_click
    # selected_point(if (!is.null(click)) click$lng else NULL) # Update selected point
  # })
  
  ### Render Leaflet map
  output$survey_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addMarkers(data = survey_locations_sf)
  })
  
  ### Render summary table based on selected point
  output$map_table <- renderTable({
    ### Check if a point is selected
    {
      ### If a point is selected, filter the data and return summary
      filtered_map_data <- filter(original_groups_weather, input$radio_tab2 == site)
      
      ### group by month_year and aggregate
      aggregated_MY_data <- filtered_map_data |>
        group_by(month_year) |>
        summarize(
          survey_site = first(site),
          survey_date = first(date),
          survey_season = first(season),
          survey_rep = first(rep),
          undaria_avg = mean(undaria_percent),
          undaria_adults_avg = mean(undaria_adults),
          undaria_juveniles_avg = mean(undaria_juveniles),
          undaria_recruits_avg = mean(undaria_recruits),
          new_sum_undaria = sum(sum_undaria_AdultsJuveniles),
          fucoids_kelps_avg = mean(percent_fucoids_kelps),
          solitary_bivalves_avg = mean(percent_solitary_bivalves),
          solitary_anemones_avg = mean(percent_solitary_anemones),
          barnacles_worms_avg = mean(percent_barnacles_worms),
          FilterFeeders_avg = mean(percent_colonial_FilterFeeders),
          reds_avg = mean(percent_reds),
          greens_avg = mean(percent_greens),
          browns_avg = mean(percent_browns),
          algal_crusts_avg = mean(percent_algal_crusts),
          red_blades_avg = mean(percent_red_blades),
          green_blades_avg = mean(percent_green_blades),
          molluscan_herbivores_avg = mean(percent_molluscan_herbivores),
          sum_crustacean_herbivores = sum(crustacean_herbivore_count),
          molluscan_predators_avg = mean(percent_molluscan_predators),
          sum_crustacean_predators = sum(crustacean_predator_count),
          sum_echninoderm_predators = sum(echinoderm_predator_count),
          coralline_turf_avg = mean(percent_coralline_turf),
          paint_avg = mean(percent_paint),
          bare_space_avg = mean(percent_bare_space),
          survey_month_year = first(month_year),
          survey_longitude = first(longitude),
          survey_latitude = first(latitude),
          survey_time = first(time),
          survey_sst = first(sst)
        )
      
      ### Select columns to display in the final table
      final_map_table <- aggregated_MY_data |>
        select(-survey_site, -survey_date, -survey_season, -survey_rep, 
                -survey_month_year, -survey_longitude, -survey_latitude, -survey_time) |>
        t()
      rownames(final_map_table) <-  colnames(aggregated_MY_data |>
                                               select(-survey_site, -survey_date, -survey_season, 
                                                      -survey_rep, 
                                                      -survey_month_year, -survey_longitude, 
                                                      -survey_latitude, -survey_time)) 
      
      return(final_map_table)
    }
  }, rownames = TRUE)
  
  # Tab 3
  output$date_table <- renderTable({
    selected_date <- input$survey_dates_tab3
    
    ### Filter data based on selected survey date
    filtered_dates <- filter(original_groups_weather, month_year == selected_date)
    
    ### Group by site and aggregate data
    aggregated_data <- filtered_dates |>
      group_by(site) |>
      summarize(
        survey_site = first(site),
        survey_date = first(date),
        survey_season = first(season),
        survey_rep = first(rep),
        undaria_avg = mean(undaria_percent),
        undaria_adults_avg = mean(undaria_adults),
        undaria_juveniles_avg = mean(undaria_juveniles),
        undaria_recruits_avg = mean(undaria_recruits),
        new_sum_undaria = sum(sum_undaria_AdultsJuveniles),
        fucoids_kelps_avg = mean(percent_fucoids_kelps),
        solitary_bivalves_avg = mean(percent_solitary_bivalves),
        solitary_anemones_avg = mean(percent_solitary_anemones),
        barnacles_worms_avg = mean(percent_barnacles_worms),
        FilterFeeders_avg = mean(percent_colonial_FilterFeeders),
        reds_avg = mean(percent_reds),
        greens_avg = mean(percent_greens),
        browns_avg = mean(percent_browns),
        algal_crusts_avg = mean(percent_algal_crusts),
        red_blades_avg = mean(percent_red_blades),
        green_blades_avg = mean(percent_green_blades),
        molluscan_herbivores_avg = mean(percent_molluscan_herbivores),
        sum_crustacean_herbivores = sum(crustacean_herbivore_count),
        molluscan_predators_avg = mean(percent_molluscan_predators),
        sum_crustacean_predators = sum(crustacean_predator_count),
        sum_echninoderm_predators = sum(echinoderm_predator_count),
        coralline_turf_avg = mean(percent_coralline_turf),
        paint_avg = mean(percent_paint),
        bare_space_avg = mean(percent_bare_space),
        survey_month_year = first(month_year),
        survey_longitude = first(longitude),
        survey_latitude = first(latitude),
        survey_time = first(time),
        survey_sst = first(sst)
      )
    
    ### Select columns to display in the final table
    final_date_table <- aggregated_data |>
      select(-survey_site, -survey_date, -survey_season, -survey_rep, 
             -survey_month_year, -survey_longitude, -survey_latitude, -survey_time) |>
      t()
    rownames(final_date_table) <- colnames(aggregated_data |>
                                        select(-survey_site, -survey_date, -survey_season, 
                                               -survey_rep, 
                                               -survey_month_year, -survey_longitude, 
                                               -survey_latitude, -survey_time))
    # browser()
    
    # Print summary of filtered data
    return(final_date_table)
  }, rownames = TRUE)

  # Tab 4
  ### Reactive expression to filter data based on user selection
  filtered_lm_data <- reactive({
    filtered_lm <- groups_long |>
      filter(variable %in% c("undaria_percent", "percent_fucoids_kelps", 
                             "percent_coralline_turf", "percent_paint")) |>
      pivot_wider(names_from = variable, values_from = count_percent)
    return(filtered_lm)
  })
  # browser()
  
  ### Reactive expression to create linear model based on user selection
  lm_model <- reactive({
    ### Extracting selected dependent variable
    dependent_variable <- input$model_options
    # browser()
    ### Creating linear model with filtered data
    lm(as.formula(paste(dependent_variable, "~ undaria_percent")), data = filtered_lm_data())
    
    # lm({{dependent_variable}} ~ undaria_percent, data = filtered_lm_data())
  })
  
  ### Render plot with graph
  output$undaria_plot <- renderPlot({
    # browser()
    ### plotting the data, fill in with reactive
    ggplot(data = filtered_lm_data(), aes(x = undaria_percent, y = !!as.symbol(input$model_options))) + 
    geom_point(size = 1) + geom_smooth(method = lm, se = TRUE)
  })
  
  ### Render summary table
  output$lm_table <- renderTable({
    # browser()
    model <- lm_model()
    # browser()
    lm_table <- summary(lm_model())
    lm_table$coefficients
  })
  
  # Tab 5
  ### Reactive expression to filter data based on selected animal
  filtered_animal_data <- reactive({
    animal <- switch(input$animals_observed,
                     "Crustacean herbivores" = "crustacean_herbivore_count",
                     "Crustacean predators" = "crustacean_predator_count",
                     "Echinoderm predators" = "echinoderm_predator_count"
    )
  })
  
  ### Render summary table for selected animal
  output$animal_table <- renderTable({
    filtered_animal_data()
  }) 
  
}

# Run the application
shinyApp(ui = ui, server = server)

# End script
