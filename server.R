######################################
## Author: Felix Busch
## Date of Authorship: 21 July 2017
######################################

## ==== load packages
library(leaflet)
library(magrittr)
library(rfigshare)
library(tidyverse)

## ==== load data

  # main data
  fs_deposit_id <- 5117287
  deposit_details <- fs_details(fs_deposit_id)
  deposit_details <- unlist(deposit_details$files)
  deposit_details <- data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)
  data <- deposit_details %>%
    filter(name == "data.csv") %>%
    select(download_url) %>%
    .[[1]] %>%
    read_delim(delim=";")
  
  # aux lists
  fs_deposit_id <- 5196268
  deposit_details <- fs_details(fs_deposit_id)
  deposit_details <- unlist(deposit_details$files)
  deposit_details <- data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F) 
  
    # seminars
    seminars <- deposit_details %>%
      filter(name == "seminars.csv") %>%
      select(download_url) %>%
      .[[1]] %>%
      read.csv(col.names="Seminare", stringsAsFactors = F, header=F)
    seminars <- seminars$Seminare
  
    # types
    types <- deposit_details %>%
      filter(name == "types.csv") %>%
      select(download_url) %>%
      .[[1]] %>%
      read.csv(col.names="Schultypen", stringsAsFactors = F, header=F)
    types <- types$Schultypen
    
    # agencies
    agencies <- deposit_details %>%
      filter(name == "agencies.csv") %>%
      select(download_url) %>%
      .[[1]] %>%
      read.csv(col.names="Aufsichtsbehörden", stringsAsFactors = F, header=F)
    agencies <- agencies$Aufsichtsbehörden

## ==== add html pop-up to data
pop_name <- paste(sep="", "<b>", data$name, "</b>")
pop_phone <- paste(sep="", "Telefon: ", "<p style='margin-left:5px; display:inline;'>", data$phone, "</p>")
pop_fax <- paste(sep="", "Fax: ", "<p style='margin-left:24px; display:inline;'>", data$fax, "</p>")
pop_email <- paste(sep="", "Email: ", "<p style='margin-left:14px; display:inline;'>", data$email, "</p>")
pop_internet <- paste(sep="", "Web: ", "<p style='margin-left:19px; display:inline;'>", "<a target='_blank' href='http://", data$internet,"'>Link</a>", "</p>")
pop_internet[is.na(data$internet)] <- paste(sep="", "Web: ", "<p style='margin-left:19px; display:inline;'>", "NA", "</p>")
pop_nkids <- paste(sep="", "Anzahl Schüler: ", "<p style='margin-left:2px; display:inline;'>", data$n_kids, "</p>")
pop_nteachers <- paste(sep="", "Anzahl Lehrer: ", "<p style='margin-left:9px; display:inline;'>", data$n_teachers, "</p>")
pop_nclasses <- paste(sep="", "Anzahl Klassen: ", "<p style='margin-left:1px; display:inline;'>", data$n_classes, "</p>")

data$popup <- paste(sep="<br/>",
                    pop_name,
                    data$street,
                    data$address,
                    pop_phone,
                    pop_fax,
                    pop_email,
                    pop_internet,
                    pop_nkids,
                    pop_nteachers,
                    pop_nclasses
  )

## ==== define server function
server <- function(input, output){
  
  # define static output map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% #add surface layer
      setView(9.107828, 48.521532, zoom = 7)
  })
  
  # static text indicating doi
  output$text_doi <- renderText("Data: <a target='_blank' href='http://doi.org/10.6084/m9.figshare.5117287'>Link</a>")
  
  # define output text
  rv <- reactiveValues()
  rv$text <- "Benutze die Suche um Schulen auf der Karte anzuzeigen."
  
  # define event listener
  observeEvent(
    input$button, # listen to click on button
    {
      
    if (input$seminar != "(kein Filter)" || input$type != "(kein Filter)" || input$agency != "(kein Filter)" || length(input$status) != 0) {
      
      # TRUE vectors
      sel1 <- vector("logical", dim(data)[1])
      sel1[1:length(sel1)] <- T
      sel2 <- vector("logical", dim(data)[1])
      sel2[1:length(sel2)] <- T
      sel3 <- vector("logical", dim(data)[1])
      sel3[1:length(sel3)] <- T
      sel4 <- vector("logical", dim(data)[1])
      sel4[1:length(sel4)] <- T
      
      # input$seminar -> logical selector
      if (input$seminar != "(kein Filter)") {

        # find index & construct seminar object name
        index <- match(input$seminar,seminars)
        index <- index-2
        varname <- paste("data$seminar_details",index,sep="")

        # write into selector
        sel1 <- grepl("1",eval(parse(text = varname)))
      }
      
      # input$type -> logical selector
      if (input$type != "(kein Filter)") {
        
        # find index & construct seminar object name
        index <- match(input$type,types)
        index <- index-2
        varname <- paste("data$type_details",index,sep="")
        
        # write into selector
        sel2 <- grepl("1",eval(parse(text = varname)))
      }      
      
      # input$agency -> logical selector
      if (input$agency != "(kein Filter)") {
        
        # find index & construct seminar object name
        index <- match(input$agency,agencies)
        index <- index-2
        varname <- paste("data$agency_details",index,sep="")
        
        # write into selector
        sel3 <- grepl("1",eval(parse(text = varname)))
      }     
      
      # input$status -> logical selector
      if (!is.null(input$status)) {
        sel4 <- grepl(paste(input$status,collapse="|"),data$status)
      }
      else { # show nothing if none is selected
        sel1[1:length(sel1)] <- F
        sel2[1:length(sel1)] <- F
        sel3[1:length(sel1)] <- F
        sel4[1:length(sel1)] <- F
      }
      
      # get choice from list as subset of data
      choice <- data[sel1 & sel2 & sel3 & sel4,]
      
      # define output text
      n_schools <- nrow(choice)
      
      if (n_schools == 0) {
        rv$text <- paste("Die Suche ergab keine Ergebnisse. Es gibt keine Schule die den Filtern entspricht.")
      }
      if (n_schools != 0) {
        rv$text <- paste("Die Suche ergab ", n_schools, " Ergebnisse. Klicke auf eine blaue Markierung um Details zu der jeweiligen Schule abzufragen.")
      }      
      
      
      # plot the subset data
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        addMarkers(lng = choice$lon,
                   lat = choice$lat,
                   popup = choice$popup) 

    } #close outer if statement
    
    else {
      # clear markers
      leafletProxy("mymap") %>%
        clearMarkers()
    }
    
  }) #close event listener
  
  # dynamic output text
  observe({
    output$text_reactive <- renderText(rv$text)
  })
  
} #close server function
