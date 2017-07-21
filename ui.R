######################################
## Author: Felix Busch
## Date of Authorship: 21 July 2017
######################################

## ==== load packages
library(leaflet)
library(rfigshare)
library(tidyverse)

## ==== load data

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

## ==== define UI
fillPage(
  
  tags$head(
    tags$style(HTML("

      .doi {
        font-size: 70%;
        margin-top: 1em;
        text-align: right
      }

      .results-text {
        margin-top: 1em;
        font-weight: bold
      }

    "))
  ),
  
  # sidebar panel includes forms
  sidebarPanel(
    
    selectInput(
      inputId = "seminar",
      label = "Seminar:",
      choices = seminars,
      width = "100%"
    ),
    
    selectInput(
      inputId = "type",
      label = "Schultyp:",
      choices = types,
      width = "100%"
    ),
    
    selectInput(
      inputId = "agency",
      label = "Aufsichtsbehörde:",
      choices = agencies,
      width = "100%"
    ),
    
    checkboxGroupInput(
      inputId = "status", 
      label = "Status",
      choices = c("Öffentliche Schule", "Private Schule", "Behörde"), 
      selected = c("Öffentliche Schule", "Private Schule"), 
      inline = F
    ),
    
    actionButton(
      inputId = "button",
      label = "Suchen"
    ),
    
    htmlOutput(outputId = "text_doi", class = "doi") #output indicating doi
    
  ),
    
  # main panel includes map
  mainPanel(
    
    leafletOutput("mymap", width="100%"), #map
    
    htmlOutput(outputId = "text_reactive", class="results-text") #reactive text output
    
  )
)