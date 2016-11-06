## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================


# institution_nodes <-
#   read.csv("data/ox-ox-nodes.csv", stringsAsFactors = F)

institution_nodes <-
  read.csv("data/ox-ox-nodes_sat.csv", stringsAsFactors = F)

# institution_edges <-
#   read.csv("data/ox-ox-edges.csv", stringsAsFactors = F)


institution_edges <- readLines("data/ox-ox-edges_sat.json", warn = F) %>%
  gather_array() %>%
  spread_values(from = jstring("Source")) %>% ## visNetwork wants from and to not source and target
  spread_values(to = jstring("Target")) %>% ## visNetwork wants from and to not source and target
  spread_values(collaborations = jstring("Collaborations")) %>%
  enter_object("authorships") %>% 
  gather_array() %>%
  spread_values(title = jstring("Title")) %>%
  spread_values(coauthors = jstring("Co-authors")) %>%
  spread_values(publication.name = jstring("Publication Name")) %>%
  spread_values(weights = jstring("Weights")) %>%
  spread_values(keywords = jstring("Keywords")) %>%
  spread_values(publication.type = jstring("Publication Type")) %>%
  spread_values(publication.date = jstring("Publication Date")) %>%
  select(from, to, title, coauthors, collaborations, publication.name, weights, keywords, publication.type, publication.date) %>%
  mutate(
    collaborations = as.numeric(collaborations),
    weights = as.numeric(weights)
  )




## colnames need to be lower case
colnames(institution_nodes) <- tolower(colnames(institution_nodes))
## the vertex tooltip is added by way of the title column:
institution_nodes$title <- institution_nodes$name
institution_nodes$title[duplicated(institution_nodes$title)]

## =========================== Duplicate Names ==================================
## ==============================================================================

## visIgraph will fail if V(igraph)$name has duplicates 
## However, for a friendly UI [dropdown with names, not IDs] we need to set $name as names
## To overcome this, append white space to labels
##
dupes <- institution_nodes$name[duplicated(institution_nodes$name)]
make_unique_names <- function(name) {
  small_vec <- as.character()
  for (i in 1:sum(dupes == name)) {
    small_vec <-
      append(x = small_vec, values = paste0(name, paste0(rep(" ", i), collapse = "")))
  }
  small_vec
}
institution_nodes$name[duplicated(institution_nodes$name)] <-
  unlist(lapply(unique(institution_nodes$name[duplicated(institution_nodes$name)]), function(x)
    make_unique_names(x)))

## =========================== Colours ==========================================
## ==============================================================================

department_colours <- data.frame(
  "department" = unique(institution_nodes$department),
  "colours" = toupper(c("#a76fce",
                        "#8e894a",
                        "#6b7af3",
                        "#bd813d",
                        "#3ebab5",
                        "#d44069",
                        "#61a5d3",
                        "#b86672",
                        "#5eb942",
                        "#bf638d",
                        "#4ca578",
                        "#8b56cf",
                        "#5da352",
                        "#d67ccd",
                        "#a3a932",
                        "#cd4295",
                        "#de9227",
                        "#746cad",
                        "#ce4c31",
                        "#5f92d9",
                        "#c86d5a",
                        "#5d74d4",
                        "#af72b1",
                        "#c34bb5"))[1:length(unique(institution_nodes$department))],
  stringsAsFactors = F
)

department_colours %>%
  arrange(department) -> department_colours

institution_nodes$color <- mapvalues(institution_nodes$department, from = department_colours$department, to = department_colours$colours,warn_missing = FALSE)

node_legend <-
  data.frame(
    label = unique(department_colours$department),
    # shape = rep("square",5),
    size =rep(10,length(department_colours$department)),
    # icon.code = c("f007","f0c0","f007"),
    icon.color = department_colours$colours,
    # icon.size = rep(5,5),
    id = 1:length(department_colours$department)
  )

## =========================== igraph ===========================================
## ==============================================================================

## id order matters for functionality including:
## - ordering of selection
## Mapping required to make reversible.



institution_igraph <-
  graph.data.frame(d = {
    institution_edges %>%
      select(from, to) %>%
      unique()
  }, vertices = institution_nodes[, 1:2])

V(institution_igraph)$title <- institution_nodes$name
V(institution_igraph)$id <- institution_nodes$id
V(institution_igraph)$color <- institution_nodes$color
V(institution_igraph)$department <- institution_nodes$department



