## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

output$institution_highchart_node_legened <- renderHighchart(
  highchart_legend(
    legend_names = department_colours$department,
    legend_colours = department_colours$colours
  )
)

output$institution_app_collapsile_info <- renderUI(fluidRow(column(
  p(paste0(
    "Number of PIs: ",
    vcount(institution_igraph)
  )),
  
  
  p(paste0("Highest Degree of Seperation: ",
           {
             shrt_paths <- shortest.paths(institution_igraph, 2)
             max(shrt_paths[shrt_paths < Inf])
           })),
  p(paste0(
    "Average Path Length: ",
    round(mean_distance(institution_igraph), digits = 2)
  )),
  width = 6
),
column(p(
  paste0("Average Degree: ", round(mean(
    degree(institution_igraph)
  ))), digits = 2
),
p(
  paste0("Number of Collaborations: ", ecount(institution_igraph))
),
width = 6)))

institution_graph <-
  eventReactive(input$institution_people_or_departments,
                switch(
                  input$institution_people_or_departments,
                  "individuals" = {
                    institution_igraph
                  },
                  "departments" = {
                    contract_instition_network(institution_igraph)
                  }
                ))

reactive_test_institution_refocus <- 

output$institution_displayed_network <- renderVisNetwork({
  institution_graph <- as.undirected(institution_graph())
  
  show(id = "loading-content", anim = TRUE, animType = "fade")

  switch (
    input$institution_people_or_departments,
    "individuals" = {
      invisible(network <- visIgraph(
        # as.undirected(departments_g),
        as.undirected(institution_graph),
        layout = "layout.forceatlas2",
        directed = FALSE,
        k = 200, # repulsion
        delta = 5, # attraction
        ks = 10, # speed constant
        ksmax = 20, # limits speed
        # autostab parameters can't be found
        gravity = 0.1,
        iterations = 200,
        nohubs = FALSE,
        randomSeed = 8
        # linlog = TRUE
      ))
      
      hide(id = "loading-content", anim = TRUE, animType = "fade")
      
      network %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = list(enabled = TRUE)
                   # values = sort(unlist(V(institution_graph)$name)))
        ) %>%
        visLayout(hierarchical = FALSE) %>%
        visInteraction(dragNodes = FALSE) %>%
        # visEdges(smooth = list("enabled" = TRUE,
        #                        "type" = "curvedCCW")) %>%
        visEvents(selectNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
                  ;}")
      
    },
    "departments" = {
      invisible(
        network <- visIgraph(
          institution_graph,
          idToLabel = FALSE
        )
      )
      
      hide(id = "loading-content", anim = TRUE, animType = "fade")
      
      network %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = list(enabled = TRUE)
                   # values = sort(unlist(V(institution_graph)$name)))
        ) %>%
        visPhysics(
          minVelocity = 1,
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(
            gravitationalConstant = -10,
            avoidOverlap = 0.8,
            centralGravity = 30),
          stabilization = list(
            "enabled" = FALSE
          )) %>%
        visLayout(hierarchical = FALSE) %>%
        visInteraction(dragNodes = FALSE) %>%
        # visEdges(smooth = list("enabled" = TRUE,
        #                        "type" = "curvedCCW")) %>%
        visEvents(selectNode = "function(nodes) {
                  Shiny.onInputChange('current_node_id', nodes);
                  ;}")
    }
    
  )
  
  })

output$institution_displayed_network_properties <- renderUI({
  wellPanel(p(paste0(
    "Average path length: ", round(average.path.length(institution_graph()), 2)
  )),
  p(paste0(
    "Number of nodes: ", vcount(institution_graph())
  )))
})

observeEvent(
  input$institution_people_or_departments,
  visNetworkProxy("institution_displayed_network") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

observeEvent(
  input$institution_refocus_network,
  visNetworkProxy("institution_displayed_network") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

output$institution_selected_node_sidePanel <- renderUI({
  if (is.null(input$institution_displayed_network_selected)) {
    return()
  }
  
  
  if (input$institution_displayed_network_selected == "") {
    return()
  }
  
  selected_id <-
    institution_nodes[institution_nodes$name == input$institution_displayed_network_selected, "id"]
  
  onClickInputCheck(
    never_Clicked = return(),
    show_Details = {

      wellPanel(
        p(strong(
          paste0(switch(
            input$institution_people_or_departments,
            "individuals" = "Selected Principal Investigator: ",
            "departments" = "Selected Department: "
          ),
          input$institution_displayed_network_selected
          ))
        ),
        
        switch(
          input$institution_people_or_departments,
          "individuals" = p(paste0("Department: ", institution_nodes[institution_nodes$name == input$institution_displayed_network_selected, "department"])),
          "departments" = p(
            paste0(
              "Members of Department: ",
              institution_nodes %>%
                filter(
                  department == input$institution_displayed_network_selected
                ) %>%
                count() %>%
                unlist(use.names = F)
            )
          )
        ),
        
        bsCollapse(
          id = "institution_degree_info",
          open = NULL,
          bsCollapsePanel(HTML(paste0(
            "<h5>",
            "Degree: ", as.numeric(degree(institution_graph())[which(V(institution_graph())$name == input$institution_displayed_network_selected)]),
            " ", "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
            "</h5>"
          )),
          fluidPage(
            HTML("
                 <p><strong>Degree</strong> (Number of co-authorships)</p>

                 <p>
                 An individual with a high score will have a more central position and likely role in the network, and will have collaborated with many others. These are likely some of the most well established researchers in the community, with knowledge, expertise and resources that have driven a large number of collaborations.  We look to connect these individuals with new researchers joining the Centre to spread their knowledge and expertise, and they form part of our senior management group.
                 </p>
                 
                 <p>
                 Has been weighted to allow for the number of authors on any given paper (more authors equals a lower relative value for that paper), and also the number of papers between two PI’s.
                 </p>
                 
                 <p>
                 A higher degree score is an accumulation of these to give a single value for each PI. The higher the value the larger the number of connections.
                 </p>")
            ), style = "default")
            ),
        
        bsCollapse(
          id = "institution_betweenness_info",
          open = NULL,
          bsCollapsePanel(HTML(paste0(
            "<h5>",
            "Betweenness: ", round(as.numeric(betweenness(
              institution_graph()
            )[which(V(institution_graph())$name == input$institution_displayed_network_selected)]), digits = 4),
            " ", "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
            "</h5>"
          )),
          fluidPage(
            HTML("
                 <p><strong>Betweeness</strong> (The number of the shortest paths that pass through a given investigator)</p>

                 <p>
                 Those with a high betweeness value often play the role of connecting different groups; spanning different communities within the network. We call these researchers boundary-spanners or super-connectors; they have the potential to bring together disparate groups and facilitate multi-disciplinary collaboration. We look to these researchers to help develop our working groups, cross-disciplinary activities, and to act as communication ambassadors for the Centre.
                 </p>
                 
                 <p>
                 They can act as brokers and connectors to bring others together, and can help to spread information and knowledge across different sub-communities within the network.
                 </p>")
          ), style = "default")
        ),
        
        bsCollapse(
          id = "institution_closeness_info",
          open = NULL,
          bsCollapsePanel(HTML(paste0(
            "<h5>",
            
            "Closeness: ", round(as.numeric(closeness(
              institution_graph()
            )[which(V(institution_graph())$name == input$institution_displayed_network_selected)]), digits = 4),
            " ", "<span class='glyphicon glyphicon-question-sign' aria-hidden='true'></span>",
            "</h5>"
          )),
          fluidPage(
            HTML("<p><strong>Closeness</strong> (the sum of the length of the shortest paths between the individual and all other individuals).</p>

                 <p>
                 Degree and closeness are interlinked, and those with a high closeness value will have a more central role in the network.
                 </p>
                 
                 <p>
                 It can be viewed as a value to assess  how long it will take information to spread from a given individual to others in the network. Those  with a high closeness value have the potential to play a role in effectively spreading information across the network.
                 </p>")
          ), style = "default")
        ),
        
        actionButton("scroll_down_institution", "Scroll down for details", width = "100%")
      )
      
      # bsCollapse(
      #   id = "institution_measure_info",
      #   open = NULL,
      #   bsCollapsePanel(HTML(
      #     "Click for details about degree, betweenness and closessness"
      #   ),
      #   fluidPage(
      #     includeMarkdown(
      #       knitr::knit("network-measure-descriptions.Rmd")
      #     )
      #   ), style = "default")
      #   )
    },
    destructive_Change = return()
  )
})

observeEvent(input$scroll_down_institution, {
  session$sendCustomMessage(type = "scrollDown", 1)
})


institution_individual_datatable <- reactive({
  if (input$institution_displayed_network_selected == "") {
    return()
  }
  selected_id <-
    institution_nodes[institution_nodes$name == input$institution_displayed_network_selected, "id"]
  subsetted_edges <-
    filter(institution_edges, from == selected_id |
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
  
  select(
    subsetted_edges,
    from,
    to,
    collaborations,
    title,
    publication.name,
    publication.date
  )
})

institution_department_datatable <- reactive({
  
  if (input$institution_displayed_network_selected == "") {
    return()
  }
  
  department_members <- filter(institution_nodes,
                               department == input$institution_displayed_network_selected) %>%
    select(id) %>%
    unlist(use.names = F)
  
  subsetted_edges <-
    filter(institution_edges,
           from %in% department_members |
             to %in% department_members)

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
  select(
    subsetted_edges,
    from,
    to,
    collaborations,
    title,
    publication.name,
    publication.date
  )
})


output$institution_selected_node_table <- DT::renderDataTable({
  if (is.null(input$institution_displayed_network_selected)) {
    return()
  }
  
  onClickInputCheck(show_Details = {
    switch(
      input$institution_people_or_departments,
      "individuals" = {
        data_to_show <- institution_individual_datatable()
        
        value_for_col_1 <-
          input$institution_displayed_network_selected
        
        ## == Modify rows where the selected individual is not in the "from" column
        rows_to_change <-
          which(data_to_show$from != value_for_col_1)

        lapply(rows_to_change, function(x) {
          first_col_value <- which(data_to_show[x, c(1, 2)] == value_for_col_1)
          colorder <-
            c(first_col_value, setdiff(1:ncol(data_to_show), first_col_value))

          data_to_show[x,] <<- data_to_show[x, colorder]
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
      "departments" = {
        data_to_show <- institution_department_datatable()
        
        data_to_show %>%
          rename_(
            "Collaborator 1" = "from",
            "Collaborator 2" = "to",
            "# of collaborations" = "collaborations",
            "Title" = "title",
            "Journal Name" = "publication.name",
            "Publication date" = "publication.date"
          )
      }
    )
  })
  
}, rownames = FALSE, extensions = "Responsive"
# container = withTags(table(class = 'display',
#                            thead(
#                              tr(
#                                th(colspan = 2, 'Collaborators')
#                                ),
#                              tr(lapply(c(
#                                "Selected PI","Collaborator","collaborations", "publication.name", "publication.date"
#                              ), th))
#                            ))))
)

output$institution_selected_node_table_UI <- renderUI({
  if (is.null(input$institution_displayed_network_selected)) {
    wellPanel("Select a node for more details")
  }
  
  
  if (input$institution_displayed_network_selected == "") {
    wellPanel("Select a node for more details")
  } else {
    onClickInputCheck(
      never_Clicked = {
        wellPanel("Select a node for more details")
      },
      show_Details = {
        wellPanel(DT::dataTableOutput("institution_selected_node_table"))
      },
      destructive_Change = wellPanel("Select a node for more details")
    )
  }
  
})