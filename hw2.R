#import libraries
library(tidyverse)
library(dplyr)
library(shiny)
library(plotly)
library(shinythemes)
library(ggplot2)

#read in the data and remove rows with NA values in relevant columns
bear_attacks = read_csv("https://raw.githubusercontent.com/mmkenton/stat436/refs/heads/main/bear_attacks.csv")

bear_attacks = bear_attacks %>% 
  filter(!is.na(Year),
         !is.na(Latitude),
         !is.na(Longitude),
         !is.na(Gender),
         !is.na(Age),
         !is.na(Name)) 
  
#create a list of bear types for the checkbox input
bear_type = pull(bear_attacks, Bear) %>% 
  unique()

#choose colors for map data points
my_colors = c('black', 'brown', 'tan', 'grey')

#define function that will be called to create map
create_map = function(df){
  plot_ly(data = df, 
          type = 'scattergeo',
          lat = ~Latitude,
          lon = ~Longitude,
          mode = "markers",
          text = ~paste(Location, ",", Year),
          hoverinfo = 'text',
          color = df$Bear,
          colors = my_colors,
          size = 3,
          marker = list(
              alpha = 0.8)) %>% 
    layout(geo = list(scope = 'north america',
                      showland = 'TRUE',
                      landcolor = "black"),
           title = 'Bear Attack Locations')
}

#define function that will be called to create graph
create_graph = function(df){
  ggplot(df, aes(x = Year, y = Gender, color = Gender, size = Age)) +
    geom_jitter() +
    scale_color_manual(values = c("pink", "skyblue")) +
    labs(x = 'Year',
         y = 'Gender', 
         title = 'Demographics of Bear Attack Victims')
}

ui = fluidPage(
  theme = shinytheme('simplex'),
  titlePanel("Bear Attacks in North America"),
  fluidRow(column(4, 
                  p("This dataset looks at bear attacks in the United States and Canada. Use the slider and checkboxes to filter the attacks. The map to the right shows the location of the attacks, and the scatterplot shows demographic information for the victims of the attcks."),
                  sliderInput("year", "Select a range of years", value = c(1901, 2018), min = 1901, max = 2018, sep = ""), 
                  checkboxGroupInput("bear_type", "Select a type of bear", bear_type, bear_type),
                  p("The table below summarizes the information about each of the attacks.")),
  column(5, plotlyOutput("map")),
  column(3, plotOutput("graph")),
  headerPanel(""),
  dataTableOutput("dt"),
  tags$head(tags$style(HTML('* {font-family: "Verdana"};')))
))

server = function(input, output){
  attack_subset = reactive({
    bear_attacks %>% 
      filter((Bear %in% input$bear_type) &
               (Year >= input$year[1]) &
               (Year <= input$year[2]))
  })
  output$map = renderPlotly(create_map(attack_subset()))
  output$dt = renderDataTable(data.frame(attack_subset()) %>% 
                                select(Name, Age, Gender, Location, Year, Bear) %>% 
                                rename("Victim's Name" = "Name", 
                                       "Victim's Age" = "Age", 
                                       "Victim's Gender" = "Gender"))
  output$graph = renderPlot(create_graph(attack_subset()))
}

shinyApp(ui, server)
