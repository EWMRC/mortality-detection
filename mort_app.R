library(shiny)
library(tidyverse)
library(leaflet)
library(excelR)


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(width =  6,
           leafletOutput(outputId = "plot",height = 700),
           h3(textOutput("compile_status"), align = "center"),
           actionButton("goButton", "Next"), p("Proof the next individual")
    ),
    column(width = 6,
           excelOutput("table", height = 400))
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### OK, lets intersect the location data with the shapefile for the AMWO 
  ### SGS coverage zones
  
  ## import datafile (this has already been run through the HMM, just sending
  ## as a csv to speed up your workflow)
  
  #amwoData.sm <- read.csv('HMMmale.csv')
  amwoData.sm <- readRDS("predicted_mortalities.rds")
  
  amwoData.sm %>%
    mutate(ID = as.character(ID)) ->
    amwoData.sm
  
  individual_stepper <- reactiveValues() #These values can be defined w/in a reactive expression and will be remembered in other reactive expressions
  individual_stepper$compiled <- 0
  individual_stepper$count <- 1
  individual_stepper$current_id <- unique(amwoData.sm$ID)[1]
  individual_stepper$amwoDataID <- amwoData.sm %>%
    filter(ID == unique(amwoData.sm$ID)[1])
  
  
  selected_columns <- filter(amwoData.sm, ID == unique(amwoData.sm$ID)[1]) %>% 
    dplyr::select("ID", "point_state", "time", "step", "angle") 
  
  output$table <- excelTable(data = selected_columns, tableHeight = "800px") %>% 
    renderExcel()
  
  getColor <- function(state) {
    sapply(state$point_state, function(states) { # individual_stepper$amwoDataID$point_state
      if(states == 1) {
        "blue"
      } else if(states == 2) {
        "red"
      } else if(states == 3){
        "pink"
      } else if(states == 4){
        "blue"
      } else {
        "red"
      } })
  }
  
  individual_stepper$icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(filter(amwoData.sm, ID == unique(amwoData.sm$ID)[1]))
  )
  
  
  #Reactive plotting: anything within this expression reruns every time input is modified
  output$plot <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     minZoom = 1, maxZoom = 22,
                                     dragging = TRUE)) %>%
      addTiles() %>% # Default base mape
      addProviderTiles("Esri.WorldImagery") %>%  # ortho image
      addProviderTiles(providers$Stamen.TonerLines) %>% # state lines and roads.
      #addProviderTiles(providers$Stamen.TonerLabels) %>% # add location and road labels
      addScaleBar() %>%
      addAwesomeMarkers(lng=individual_stepper$amwoDataID$x, 
                        lat=individual_stepper$amwoDataID$y, 
                        icon=individual_stepper$icons,
                        popup = individual_stepper$amwoDataID$date) %>%  
      addPolylines(lng =individual_stepper$amwoDataID$x, 
                   lat = individual_stepper$amwoDataID$y, 
                   weight=3, color="red")
    
    
  })#end of reactive plotting
  
  #clean locations that we start with
  #  location_iterator <- reactive({
  #amwoData.sm %>%  filter(ID == individual_stepper$current_ID)
  #  }) 
  
  #updating when go button is pressed
  observeEvent(input$goButton, {
    # Check if we've reached the end. If so, compile. If not, advance
    if(individual_stepper$count != length(unique(amwoData.sm$ID))){
      individual_stepper$count <- individual_stepper$count + 1
      individual_stepper$current_id <- unique(amwoData.sm$ID)[individual_stepper$count]
      print(individual_stepper$current_id)
    }
    else{
      individual_stepper$compiled <- 1
    }
    
    individual_stepper$amwoDataID <- subset(amwoData.sm, amwoData.sm$ID==individual_stepper$current_id)
    
    selected_columns <- individual_stepper$amwoDataID %>% 
      dplyr::select("ID", "point_state", "time", "step", "angle") 
    
    output$table <- renderExcel(excelTable(data = selected_columns, tableHeight = "800px"))
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(individual_stepper$amwoDataID)
    )
  })
  
  #  observeEvent(input$goButton,{
  #  })individual_stepper$current_id
  
  output$compile_status <- renderText({
    if(individual_stepper$compiled == 0){
      individual_stepper$current_id
    } else{
      "All individuals parsed"
    }
  })
}#end of server call

# Run the application 
shinyApp(ui = ui, server = server)