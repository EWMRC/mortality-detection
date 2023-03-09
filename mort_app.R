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
  
  ids_detected_mortality <- amwoData.sm %>% 
    filter(is.na(known)) %>% 
    mutate(ID = as.character(ID)) %>% 
    filter(point_state == 3) %>% 
    pull(ID) %>% 
    unique()
  
  paste0("Number of birds with detected mortalities: ", length(ids_detected_mortality)) %>% 
    print()
  
  amwoData.sm %>%
    filter(is.na(known)) %>% #don't examine the training dataset
    mutate(ID = as.character(ID)) %>% 
    filter(ID %in% ids_detected_mortality) -> 
    #filter(ID == "AL-2020-01") ->
    amwoData.sm
  
  individual_stepper <- reactiveValues() #These values can be defined w/in a reactive expression and will be remembered in other reactive expressions
  individual_stepper$compiled <- 0
  individual_stepper$count <- 1
  individual_stepper$current_id <- unique(amwoData.sm$ID)[1]
  individual_stepper$amwoDataID <- amwoData.sm %>%
    dplyr::filter(ID == unique(amwoData.sm$ID)[1]) %>% 
    dplyr::select("ID", "point_state", "time", "x", "y") 
  individual_stepper$catch_df <- data.frame()
  
  # individual_stepper$new_locations <- dplyr::filter(amwoData.sm, ID == unique(amwoData.sm$ID)[1]) %>% 
  #   dplyr::select("ID", "point_state", "time", "x", "y") 
  
  output$table <- excelTable(data = individual_stepper$amwoDataID, tableHeight = "800px") %>% 
    renderExcel()
  
  getColor <- function(state) {
    sapply(state$point_state, function(states) { # individual_stepper$amwoDataID$point_state
      if(states == 1) {
        "blue"
      } else if(states == 2) {
        "orange"} 
        else {
        "red"
      } }) %>% unname() #to fix the JSON bug
  }
  
  individual_stepper$icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(dplyr::filter(amwoData.sm, ID == unique(amwoData.sm$ID)[1]))
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
                        popup = individual_stepper$amwoDataID$time) %>%  
      addPolylines(lng =individual_stepper$amwoDataID$x, 
                   lat = individual_stepper$amwoDataID$y, 
                   weight=3, color="red")
    
    
  })#end of reactive plotting
  
  #updating when go button is pressed
  observeEvent(input$goButton, {
    
    individual_stepper$catch_df <- rbind(individual_stepper$catch_df, individual_stepper$amwoDataID)
    
    # Check if we've reached the end. If so, compile. If not, advance
    if(individual_stepper$count != length(unique(amwoData.sm$ID))){
      individual_stepper$count <- individual_stepper$count + 1
      individual_stepper$current_id <- unique(amwoData.sm$ID)[individual_stepper$count]
      print(individual_stepper$current_id)
      
      individual_stepper$amwoDataID <- subset(amwoData.sm, amwoData.sm$ID==individual_stepper$current_id) %>% 
        dplyr::select("ID", "point_state", "time", "x", "y")
      
      output$table <- individual_stepper$amwoDataID %>% 
        renderExcel(excelTable(data = ., tableHeight = "800px"))
      
      individual_stepper$icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(individual_stepper$amwoDataID)
      )
    }
    else{
      #Write the resulting file here
      individual_stepper$catch_df %>%
        dplyr::transmute(ID = ID,
                         time = time,
                         mortality_signal = ifelse(point_state %in% c(1,2), 0, 1)) %>% 
        readr::write_csv(file = "mortality_data_movebank_upload.csv", 
                  na = "", 
                  eol = "\r\n")
      
      individual_stepper$compiled <- 1
    }
  })
  
  #Overwrite new_locations and colors with user inputs when edited
  observeEvent(input$table,{
    individual_stepper$amwoDataID <- excel_to_R(input$table)
    
    individual_stepper$icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(individual_stepper$amwoDataID))
  })
  
  output$compile_status <- renderText({
    if(individual_stepper$compiled == 0){
      #individual_stepper$current_id

      if(any(individual_stepper$amwoDataID$point_state == 3)){
        centroid <- individual_stepper$amwoDataID %>% 
          filter(point_state == 3) %>%
          summarise(x = mean(x), y = mean(y)) %>% 
          sf::st_as_sf(coords = c("x", "y"), crs = 4326)
        
        threshold_dist <- individual_stepper$amwoDataID %>% 
          filter(point_state == 3) %>%
          sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
          st_distance(centroid) %>% 
          as.numeric() %>% 
          quantile(probs = 0.5)
      } else{
        threshold_dist <- 0
      }
      
      paste0("50% threshold: ", threshold_dist, " m")
      
    } else{
      "Mortality data compiled: ready for upload"
    }
  })
  
  # session$onSessionEnded(function() { # when the window closes, stop the app so that we can run new code
  #   stopApp()
  # })
}#end of server call

# Run the application 
shinyApp(ui = ui, server = server)