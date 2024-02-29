library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(rlang)
library(shiny)
library(shinyjs)
library(tibble)
library(tidyr)
library(zoo)


# general configuration
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
TESTING = TRUE
SEED = 42
set.seed(SEED)


# create basic data frame
# makes program slow! For faster results, set TESTING=TRUE to use precompiled subset of the data
create_df <- function() {
  # load all data
  df_measure <- read.csv("../data/train.csv")
  df_weather <- read.csv("../data/weather_train.csv")
  df_building <- read.csv("../data/building_metadata.csv")
  
  # join data 
  df_raw <- df_measure %>%
    left_join(df_building, by = "building_id") %>%
    left_join(df_weather, by = c("site_id", "timestamp"))
  
  # filter by electricity data
  df <- df_raw %>%
    filter(`meter` == 0) %>%
    mutate(timestamp = ymd_hms(timestamp))
  
  if (TESTING) {
    unique_buildings <- unique(df$building_id)
    building_sample <- sample(unique_buildings, 20)
    
    df <- df %>%
      filter(`building_id` %in% building_sample) 
  }
  
  return (df)
}


# loads precompiled dataset
load_dummy <- function() {
  df <- read.csv("dummy_data.csv") %>%
    mutate(`timestamp` = ymd_hms(timestamp))
}


# maps feature_id to human readable description
get_feature_description_dict <- function() {
  return (
    c(
      "meter_reading" = "Electricity Usage (kWh)",
      "air_temperature" = "Air Temperature (°C)",
      "cloud_coverage" = "Cloud Coverage (%)",
      "dew_temperature" = "Dew Temperature (°C)",
      "precip_depth_1_hr" = "Precipitation (ml / mm^2)",
      "sea_level_pressure" = "Sea Level Pressure (hPa)",
      "wind_direction" = "Wind Direction (Degrees)",
      "wind_speed" = "Wind Speed (km/h)"
    )
  )
}


# maps human readable description to feature_ids
get_description_feature_dict <- function() {
  return (
    c(
      "Electricity Usage (kWh)" =  "meter_reading",
      "Air Temperature (°C)" =  "air_temperature",
      "Cloud Coverage (%)" =  "cloud_coverage",
      "Dew Temperature (°C)" =  "dew_temperature",
      "Precipitation (ml / mm^2)" =  "precip_depth_1_hr",
      "Sea Level Pressure (hPa)" =  "sea_level_pressure",
      "Wind Direction (Degrees)" =  "wind_direction",
      "Wind Speed (km/h)" =  "wind_speed"
    )
  )
}


# map human readable description to feature_ids for meta data
get_description_feature_dict_meta_data <- function() {
  return (
    c(
      "Building Nr" = "building_id",
      "Site Nr" = "site_id",
      "Build Year" = "year_built",
      "Area (sq ft)" = "square_feet",
      "Floors" = "floor_count",
      "Primary Use" = "primary_use"
    )
  )
}


# plots given feature for given buildings
plot_buildings <- function(df, building_ID, feature_ID, smooth, roll_window) {
  df <- df %>%
    filter(`building_id` %in% building_ID)
  
  df %>%
    as_tibble() %>%
    print(n=5,width=Inf)
  if(smooth) {
    df <- df %>%
      group_by(building_id) %>%
      mutate(roll = rollmean(get({{feature_ID}}), roll_window, fill=NA)) %>%
      ungroup()
    
    df[feature_ID] <- df["roll"]
    
    df %>%
      as_tibble() %>%
      print(n=5,width=Inf)
    }
  
  plt <- ggplot(df,
                aes_string(y = feature_ID,
                           x = "timestamp")) + 
    geom_line(aes(color = as.factor(building_id))) +
    labs(color = "Building Number") +
    xlab("Time") +
    ylab(get_feature_description_dict()[feature_ID])
  
  
  return (plt)
}


# summarises meta data of given buildings
get_building_summary <- function(df, building_IDs) {
  df <- df %>% 
    filter(`building_id` %in% building_IDs) %>%
    group_by(building_id, 
             site_id, 
             year_built,
             square_feet,
             floor_count,
             primary_use) %>%
    summarise() %>%
    rename(get_description_feature_dict_meta_data())
  
  return (df)
}


# create or load our dataset
df <- data.frame()

if (!TESTING) {
  df <- create_df()
  summary(df)
} else {
  df <- load_dummy()
  summary(df)
}


# generate Shiny UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # title
  titlePanel("Welcome to explore our data"),
  hr(),
  
  # Short instructions
  textOutput(outputId = "instruction_out"),
  hr(),
  
  # create side panel for user input
  sidebarPanel(
    h4("Input"),
    selectInput(inputId = "building_IDs",
                label = "Select buildings:",
                unique(df$building_id),
                multiple = TRUE),
    
    selectInput(inputId = "feature_ID",
                label = "Select a feature:",
                get_description_feature_dict()),
    
    checkboxInput(inputId = "smooth_ID",
                  label = "Smoothen Graph"),
    
    sliderInput(inputId = "roll_window_ID",
                label = "Window for smoothing",
                value = 25,
                min = 5, 
                max = 250)
  ),
  
  # create main panel for data visualisation
  mainPanel(
    h4("Data Plot"),
    plotOutput("plot"),
    
    h4("Summary of Buildings"),
    tableOutput("table")
  )
)


# create Shiny server
server <- function(input, output, session) {
  
  # disable slider if no smoothing
  observeEvent(input$smooth_ID,
               {
                 if(input$smooth_ID) {
                   shinyjs::enable("roll_window_ID")
                 } else {
                   shinyjs::disable("roll_window_ID")
                 }
               })
  
  # Set instruction text
  output$instruction_out <- renderText({
    "Please select some buildings you want to compare!"
  })
  
  # create plot for buildings
  output$plot <- renderPlot({
    plot_buildings(df,
                   input$building_IDs,
                   input$feature_ID,
                   input$smooth_ID,
                   input$roll_window_ID)
  })
  
  # create summary of buildings
  output$table <- renderTable(get_building_summary(df, 
                                                   input$building_IDs))
}

# run the Shiny app
shinyApp(ui, server)
