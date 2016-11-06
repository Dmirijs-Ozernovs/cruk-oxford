## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #000000;
}
"

library(shiny)
library(igraph)
library(visNetwork)
library(highcharter)
library(shinyBS)
library(shinyjs)

shinyUI(
  navbarPage(
    tags$head(
      tags$script(src = "iframeResizer.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.5.2/animate.min.css"),
      tags$script(
        '
        Shiny.addCustomMessageHandler("scrollDown",
        function(color) {
        var y = $(window).scrollTop();  //your current y position on the page
        $(window).scrollTop(y+200);
        }
        );'
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "tooltip.css")
      ),
    "",
    tabPanel(
      "Welcome",
      fluidPage(
        includeMarkdown(
          knitr::knit("welcome-page.Rmd")
        )
      )
    ),
    tabPanel(
      "Centre",
      fluidPage(
        useShinyjs(),
        inlineCSS(appCSS),
        wellPanel(
          includeMarkdown(
            knitr::knit("centre-tab_top-description.Rmd")
          )
        ),
        bsCollapse(
          id = "collapseExample",
          open = NULL,
          bsCollapsePanel(HTML(
            paste0(
              '<span class="glyphicon glyphicon-plus" aria-hidden="true"></span>',
              "Institution Overview (click to expand)"
            )
          ),
          fluidPage(
            uiOutput("institution_app_collapsile_info")
          ), style = "primary")
        ),
        fluidRow(
          column(
            wellPanel(
              selectInput(
                "institution_people_or_departments",
                label = "Display:",
                choices = c("individuals", "departments")
              )
            ),
            uiOutput("insitution_displayed_network_properties"),
            uiOutput("institution_selected_node_sidePanel"),
            width = 4
          ),
          column(
            div(id = "loading-content",
                fluidPage(
                  h2(class = "animated infinite pulse", "Gathering data for network..."),
                  HTML("<img src=images/cruk-logo.png width='50%'></img>")
                )),
            visNetworkOutput("institution_displayed_network", width = "100%"),
            actionButton("institution_refocus_network", "Refocus Network", width = "100%"),
            width = 8
          )
        ),
        highchartOutput("institution_highchart_node_legened", height = "200px"),
        uiOutput("institution_selected_node_table_UI")
        # wellPanel(
        #   DT::dataTableOutput("selected_node_table")
        # )
        
      )
    ),
    tabPanel(
      "Department",
      fluidPage(
        uiOutput("select_department_UI"),
        uiOutput("department_app_title"),
        uiOutput("department_app_description"),
        bsCollapse(
          id = "collapseExample",
          open = NULL,
          bsCollapsePanel(HTML(
            paste0(
              '<span class="glyphicon glyphicon-plus" aria-hidden="true"></span>',
              " Deparment Overview (click to expand)"
            )
          ),
          fluidPage(
            uiOutput("department_app_collapsile_info")
          ), style = "primary")
        ),
        tabsetPanel(
          tabPanel(
            "People Directory",
            uiOutput("department_people_directory_UI")
          ),
          tabPanel(
            "Department Collaboration Network",
            fluidPage(
              fluidRow(
                column(
                  selectInput(
                    "people_or_departments",
                    "Show?",
                    choices = c("within department", "within whole network")
                  ),
                  uiOutput("department_network_edge_degree_UI"),
                  uiOutput("department_selected_node_sidePanel"),
                  width = 4
                ),
                column(
                  visNetworkOutput("department_displayed_network"),
                  actionButton("department_refocus_network", "Refocus Network", width = "100%"),
                  width = 8
                )
              ),
              highchartOutput("department_highchart_node_legened", height = "200px"),
              uiOutput("department_selected_node_table_UI")
            )
          )
        )
      )
    ),
    tabPanel(
      "Principal Investigators",
      fluidPage(
        wellPanel(
          "This page provides a directory of all PIs in the institution, note that at present the data is quite minimal as we are awaiting in change in the structure of data to make it easier for all of an individual's interactions to be seen at once. For demonstrative purposes, a computation of the number of interactions in database has been made and is displayed below."
        ),
        DT::dataTableOutput("PI_people_directory")
      )
    )
    
    
    ,
    collapsible = TRUE
  )
  )