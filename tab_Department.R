## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================


## ================= selected department details  ===============================
## ==============================================================================

output$select_department_UI <- renderUI(
  selectInput(
    "selected_department",
    label = "Select Department",
    choices = sort(unique(institution_nodes$department)),
    width = "100%"
  )
)


output$department_app_title <-
  renderUI(h1(paste(input$selected_department, "Profile")))

output$department_app_description <- renderUI(wellPanel(
  includeMarkdown(
    knitr::knit("department-tab_top-description.Rmd")
  )
))

output$department_app_collapsile_info <- renderUI({
  if (is.null(input$selected_department)) {
    return()
  }
  
  
  department_graph <- induced_subgraph(institution_igraph,
                                       institution_nodes[institution_nodes$department == input$selected_department, "name"])
  
  fluidRow(column(p(paste0(
    "Number of PIs: ",
    vcount(department_graph)
  )),
  
  
  p(
    paste0("Highest Degree of Seperation: ",
           {
             shrt_paths <- shortest.paths(department_graph, 2)
             max(shrt_paths[shrt_paths < Inf])
           })
  ),
  p(paste0(
    "Average Path Length: ",
    round(mean_distance(department_graph), digits = 2)
  )),
  width = 6),
  column(p(
    paste0("Average Degree: ", round(mean(
      degree(department_graph())
    ))), digits = 2
  ),
  p(
    paste0("Number of Collaborations: ", ecount(department_graph))
  ),
  width = 6))
})


## ================= People Directory DT/UI =====================================
## ==============================================================================

output$department_people_directory_DT <- DT::renderDataTable({
  # institution_nodes[institution_nodes$department == input$selected_department, c("name", "institution", "department")]
  #
  
  department_graph <- as.undirected(department_graph())
  
  institution_nodes %>%
    filter(department == input$selected_department) %>%
    select(name, department) %>%
    arrange(name) %>%
    mutate(
      Degree = as.numeric(revalue(name, degree(department_graph))),
      Betweeness = as.numeric(revalue(name, betweenness(department_graph))),
      Closeness = as.numeric(revalue(name, round(closeness(department_graph), digits = 4)))
    ) %>%
    rename(Name = name, Department = department)
  
}, rownames = FALSE,
# filter = FALSE,
escape = FALSE,
extensions = "Responsive",
options = list("language" = list("sSearch" = "Filter:")))

output$department_people_directory_UI <- renderUI(fluidPage(
  paste("These are the people in the ", input$selected_department),
  DT::dataTableOutput("department_people_directory_DT")
))

## ================= Department Network DT/UI =====================================
## ==============================================================================

department_graph <-
  eventReactive(
    c(
      input$selected_department,
      input$people_or_departments,
      input$vertex_degree
    ),
    switch(
      input$people_or_departments,
      "within department" = {
        departmental_nodes <-
          institution_nodes[institution_nodes$department == input$selected_department, "name"]
        
        induced_subgraph(institution_igraph, departmental_nodes)
        
      },
      
      "within whole network" = {
        graph.union(make_ego_graph(institution_igraph,
                                   order = 2,
                                   nodes = institution_nodes[institution_nodes$department == input$selected_department, "name"])) %>% igraph_deduplicate_vertex_attr()
      },
      
      ## Note that this does not work, hence unavailable in options
      "unavailable_department_level_interactions" = {
        if (is.null(input$vertex_degree)) {
          return()
        }
        department_ego_networks <-
          graph.union(
            make_ego_graph(
              institution_igraph,
              order = input$vertex_degree,
              nodes = institution_nodes[institution_nodes$department ==
                                          input$selected_department, "name"]
            )
          )
        
        
        igraph_deduplicate_vertex_attr(department_ego_networks)
        
      }
    )
  )

output$department_network <- renderVisNetwork({
  if (input$people_or_departments == "within whole network" &
      is.null(input$vertex_degree)) {
    return()
  }
  department_graph <- as.undirected(department_graph())
  
  
  switch(
    input$people_or_departments,
    "within department" = {
      visIgraph(department_graph,
                idToLabel = F)
    },
    "within whole network" = {
      visIgraph(department_graph,
                idToLabel = F,
                layout = "layout_with_lgl")
    }
  ) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE)) %>%
    visLayout(hierarchical = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  })

output$department_network_edge_degree_UI <- renderUI({
  if (input$people_or_departments == "within whole network") {
    wellPanel(
      sliderInput(
        "vertex_degree",
        label = "Vertex Degree",
        min = 1,
        max = 5,
        step = 1,
        value = 1
      )
    )
  }
})


## ================= Legend =====================================
## ==============================================================================

output$department_highchart_node_legened <- renderHighchart(
  highchart_legend(
    legend_names = department_colours$department,
    legend_colours = department_colours$colours
  )
)



## =========================== Generate Graph ====================================
## ==============================================================================




output$department_displayed_network <- renderVisNetwork({
  department_graph <- as.undirected(department_graph())
  
  visIgraph(department_graph,
            idToLabel = F,
            layout = "layout_nicely") %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE)) %>%
    visLayout(hierarchical = FALSE) %>%
    visInteraction(dragNodes = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  })
output$department_displayed_network_properties <- renderUI({
  wellPanel(p(paste0(
    "Average path length: ", round(average.path.length(department_graph()), 2)
  )),
  p(paste0(
    "Number of nodes: ", vcount(department_graph())
  )))
})

observeEvent(
  input$department_refocus_network,
  visNetworkProxy("department_displayed_network") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

output$department_selected_node_sidePanel <- renderUI({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  if (input$department_displayed_network_selected == "") {
    return()
  }
  onClickInputCheck(
    never_Clicked = return(),
    show_Details = {
      wellPanel(
        p(strong("Individual Stats")),
        p(
          paste0(
            "Selected Principal Investigator: ",
            input$department_displayed_network_selected
          )
        ),
        
        p(paste0("Department: ", institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "department"])),
        
        
        bsCollapse(
          id = "department_degree_info",
          open = NULL,
          bsCollapsePanel(HTML(
            paste0(
              "<h5>",
              "Degree: ",
              as.numeric(degree(department_graph())[which(V(department_graph())$name == input$department_displayed_network_selected)]),
              " ",
              "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
              "</h5>"
            )
          ),
          fluidPage(
            HTML(
              "
              <p><strong>Degree</strong> (Number of co-authorships)</p>
              
              <p>
              An individual with a high score will have a more central position and likely role in the network, and will have collaborated with many others. These are likely some of the most well established researchers in the community, with knowledge, expertise and resources that have driven a large number of collaborations.  We look to connect these individuals with new researchers joining the Centre to spread their knowledge and expertise, and they form part of our senior management group.
              </p>
              
              <p>
              Has been weighted to allow for the number of authors on any given paper (more authors equals a lower relative value for that paper), and also the number of papers between two PI’s.
              </p>
              
              <p>
              A higher degree score is an accumulation of these to give a single value for each PI. The higher the value the larger the number of connections.
              </p>"
            )
            ), style = "default")
            ),
        
        bsCollapse(
          id = "department_betweenness_info",
          open = NULL,
          bsCollapsePanel(HTML(
            paste0(
              "<h5>",
              "Betweeness: ",
              round(as.numeric(betweenness(
                department_graph()
              )[which(V(department_graph())$name == input$department_displayed_network_selected)]), digits = 4),
              " ",
              "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
              "</h5>"
            )
          ),
          fluidPage(
            HTML(
              "
              <p><strong>Betweeness</strong> (The number of the shortest paths that pass through a given investigator)</p>
              
              <p>
              Those with a high betweeness value often play the role of connecting different groups; spanning different communities within the network. We call these researchers boundary-spanners or super-connectors; they have the potential to bring together disparate groups and facilitate multi-disciplinary collaboration. We look to these researchers to help develop our working groups, cross-disciplinary activities, and to act as communication ambassadors for the Centre.
              </p>
              
              <p>
              They can act as brokers and connectors to bring others together, and can help to spread information and knowledge across different sub-communities within the network.
              </p>"
            )
            ), style = "default")
            ),
        
        bsCollapse(
          id = "department_closeness_info",
          open = NULL,
          bsCollapsePanel(HTML(
            paste0(
              "<h5>",
              "Closeness: ",
              round(as.numeric(closeness(
                department_graph()
              )[which(V(department_graph())$name == input$department_displayed_network_selected)]), digits = 4),
              " ",
              "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
              "</h5>"
            )
          ),
          fluidPage(
            HTML(
              "<p><strong>Closeness</strong> (the sum of the length of the shortest paths between the individual and all other individuals).</p>
              
              <p>
              Degree and closeness are interlinked, and those with a high closeness value will have a more central role in the network.
              </p>
              
              <p>
              It can be viewed as a value to assess  how long it will take information to spread from a given individual to others in the network. Those  with a high closeness value have the potential to play a role in effectively spreading information across the network.
              </p>"
            )
            ), style = "default")
            ),
        
        actionButton("scroll_down_department", "Scroll down for details", width = "100%")
            )
    },
    destructive_Change = return()
          )
})

observeEvent(input$scroll_down_department, {
  session$sendCustomMessage(type = "scrollDown", 1)
})


department_within_department_table <- reactive({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  if (input$department_displayed_network_selected == "") {
    return()
  }
  
  department_members <- V(department_graph())$id
  
  subsetted_edges <-
    filter(institution_edges,
           from %in% department_members &
             to %in% department_members)
  
  selected_id <-
    institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"]
  
  
  subsetted_edges <-
    filter(subsetted_edges, from == selected_id |
             to == selected_id)
  
  
  subsetted_edges$from <-
    mapvalues(
      subsetted_edges$from,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  subsetted_edges$to <-
    mapvalues(
      subsetted_edges$to,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  
  subsetted_edges %>%
    select(from,
           to,
           title,
           collaborations,
           publication.name,
           publication.date)
})

department_within_whole_table <- reactive({
  if (input$department_displayed_network_selected == "") {
    return()
  }
  
  department_members <- V(department_graph())$id
  
  subsetted_edges <-
    filter(institution_edges,
           from %in% department_members &
             to %in% department_members)
  
  
  selected_id <-
    institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"]
  
  
  subsetted_edges <-
    filter(subsetted_edges, from == selected_id |
             to == selected_id)
  
  subsetted_edges$from <-
    mapvalues(
      subsetted_edges$from,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  subsetted_edges$to <-
    mapvalues(
      subsetted_edges$to,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  
  subsetted_edges %>%
    select(from,
           to,
           title,
           collaborations,
           publication.name,
           publication.date)
})

output$department_selected_node_table <- DT::renderDataTable({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  selected_node_table <- onClickInputCheck(show_Details = {
    switch(
      input$people_or_departments,
      "within department" = {
        data_to_show <- department_within_department_table()
        
        value_for_col_1 <-
          input$department_displayed_network_selected
        
        ## == Modify rows where the selected individual is not in the "from" column
        rows_to_change <-
          which(data_to_show$from != value_for_col_1)
        
        lapply(rows_to_change, function(x) {
          first_col_value <-
            which(data_to_show[x, c(1, 2)] == value_for_col_1)
          colorder <-
            c(first_col_value, setdiff(1:ncol(data_to_show), first_col_value))
          
          data_to_show[x, ] <<- data_to_show[x, colorder]
        })
        ## == END
        
        data_to_show %>%
          rename_(
            "Selected PI" = "from",
            "Collaborator" = "to",
            "# of collaborations" = "collaborations",
            "Title" = "title",
            "Journal Name" = "publication.name",
            "Publication date" = "publication.date"
          )
      },
      "within whole network" = {
        data_to_show <- department_within_whole_table()
        
        value_for_col_1 <-
          input$department_displayed_network_selected
        
        print("before to change")
        rows_to_change <-
          which(data_to_show$from != value_for_col_1)
        print("after to change")
        lapply(rows_to_change, function(x) {
          first_col_value <-
            which(data_to_show[x, c(1, 2)] == value_for_col_1)
          colorder <-
            c(first_col_value, setdiff(1:ncol(data_to_show), first_col_value))
          
          data_to_show[x, ] <<- data_to_show[x, colorder]
        })
        
        data_to_show %>%
          rename_(
            "Selected PI" = "from",
            "Collaborator" = "to",
            "# of collaborations" = "collaborations",
            "Title" = "title",
            "Journal Name" = "publication.name",
            "Publication date" = "publication.date"
          )
      }
    )
  })
  
  selected_node_table
}, extensions = "Responsive")

output$department_selected_node_table_UI <- renderUI({
  if (input$department_displayed_network_selected == "") {
    wellPanel("Select a node for more details")
  } else {
    onClickInputCheck(
      never_Clicked = {
        wellPanel("Select a node for more details")
      },
      show_Details = {
        wellPanel(DT::dataTableOutput("department_selected_node_table"))
        # print(institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"])
      },
      destructive_Change = wellPanel("Select a node for more details")
    )
  }
  
})
