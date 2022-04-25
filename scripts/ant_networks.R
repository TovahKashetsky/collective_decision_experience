## setwd("C:/Users/jy33/OneDrive/Desktop/R/ant_sna")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(igraph)

# Script that allows igraph plots to change arrow size
source("scripts/igraphplot2.R")
environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')

###################### INPUTTING AND ORGANIZING DATA ########################
all_data <- read.csv("data/all_data_raw.csv") %>% 
    filter(behaviour %in% c("transport", "tandem_run", "reverse_tandem_run")) %>% # Removes trial starts and discoveries
    filter(successful != "failed") %>% # Removes failed TRs and RTRs 
    filter(focal != "" & focal != "new_worker") %>% # Removes new workers and unknown workers
    filter(partner != "" & partner != "new_worker") %>% # Removes new workers and unknown workers
    filter(!(origin == "dark" & destination == "dim")) %>%  # Removes indirect interactions
    filter(!(origin == "dim" & destination == "dark")) %>% # Removes indirect interactions
    filter(!(origin == "dim" & destination == "dim")) %>% # Removes indirect interactions
    filter(!(origin == "dark" & destination == "dark")) %>% # Removes indirect interactions
    select(Group, Colony, Trial, behaviour, focal, partner)
            
################# NETWORKS CREATION FUNCTIONS######################
# Function that creates igraph objects
func_igraph <- function(trial_list){
  
    edge_list <- trial_list %>% 
    select(focal, partner) %>% 
    mutate(weight = 1) # turns dataframes for each rep into edgelists
  
    igraph <- graph_from_data_frame(d = edge_list, vertices = nodes) # creates igraph objects
  
    strength <- strength(igraph) # calculate strength
    igraph <- set_vertex_attr(igraph, "strength", value = strength) # assign strength as vertex attributes
  
    igraph <- simplify(igraph, remove.multiple = TRUE, edge.attr.comb = sum) # remove double edges
  
  return(igraph)
}

# Function that assigns igraph attributes
func_plot_igraph <- function(igraph){
    V(igraph)$size <- (V(igraph)$strength+2)*4
    V(igraph)$label.color <- "black"
    V(igraph)$color <- "white"
    E(igraph)$color <- "black"
    E(igraph)$width <- E(igraph)$weight*3
  return(igraph)
}

############### NO CHOICE COLONIES ##############
c42 <- all_data %>% 
       filter(Colony == "n.amb42")

c42_list <- split(c42, c42$Trial) # Creates a list for the three trials

## Colony 42
igraphs_42 <- lapply(c42_list, func_igraph) 
igraphs_42 <- lapply(igraphs_42, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_42[[1]])
coords_42 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_42) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

# Trial #2 
id <- tkplot(igraphs_42[[2]])
tkplot(igraphs_42[[2]])

tk_set_coords(id, coords_42) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

# Trial #3
id <- tkplot(igraphs_42[[3]])

tk_set_coords(id, coords_42) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

