## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

library(igraph)
library(visNetwork)
library(plyr)
library(dplyr)
library(shiny)
library(ForceAtlas2) # devtools::install_github("analyxcompany/ForceAtlas2")
library(igraph)
library(DT)
library(highcharter)
library(tidyr)
library(shinyjs)
library(jsonlite)
library(tidyjson)

source("shared_graph_functions.R", local = T)
source("beautification.R", local = T)
source("data-processing.R", local = T)



capwords <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## =========================== Server Function ==================================
## ==============================================================================

shinyServer(function(input, output, session) {
  
  
  source("control_tracker.R", local = TRUE)$value
  
  source("tab_Department.R", local = TRUE)$value
  
  source("tab_Institution.R", local = TRUE)$value
  
  source("tab_PrincipalInvestigators.R", local = TRUE)$value
  
})