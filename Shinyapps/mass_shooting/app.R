#the data has the dates formatted with 2 and 4 digit years
#credit for this function goes to
#https://stackoverflow.com/questions/14893364/handling-dates-with-mix-of-two-and-four-digit-years
library(tidyverse)
library(leaflet)

multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[i]][a[[i]]>Sys.Date() | a[[i]]<as.Date("1000-01-01")]<-NA
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}

mass_shootings<-read_csv("MJ_mass_shooting.csv") %>% 
  mutate(longitude=longitude %>% as.numeric()) %>% 
  mutate(latitude=latitude %>% as.numeric()) %>% 
  mutate(date=multidate(date, c("%m/%d/%y","%m/%d/%Y")))

text_about<-c("Mother Jones collects data for mass shootings in the United States",
              "(available at https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/)",
              "The data ranges from 1982 to 2022. The size of the circles is proportional to the number of fatalities.",
              "Clicking on a circle displays a summary of the event excerpted from news coverage.",
              "This Shiny app was built as an extension of a project from DataCamp.")

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                dateRangeInput(
                  'date_range', 'Select Date', "1982-01-01", "2022-12-01"
                ),
                # Add an action button to display About info
                actionButton('show_about', "About")
  ),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
)

server <- function(input, output, session) {
  # observeEvent to display a modal dialog
  # with the help text stored in text_about.
  observeEvent(input$show_about, {
    showModal(modalDialog(title='About', div(text_about[1], style="font-size:120%"),
                          div(text_about[2], style="font-size:100%"),
                          div(text_about[3], style="font-size:100%"),
                          div(text_about[4], style="font-size:100%"),
                          div(text_about[5], style="font-size:100%")))
  })
  
  
  output$map <- leaflet::renderLeaflet({
    mass_shootings %>% 
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities
      ) %>% 
      leaflet::leaflet() %>% 
      leaflet::setView( -98.58, 39.82, zoom = 4) %>% 
      leaflet::addTiles() %>% 
      leaflet::addCircleMarkers(
        popup = ~ summary, radius = ~ sqrt(fatalities)*3,
        fillColor = 'red', color = 'red', weight = 1
      )
  })
}

shinyApp(ui, server)