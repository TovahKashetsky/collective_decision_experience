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
    filter(!(focal %in% c("new_worker", "", "w__", "unkown_ID"))) %>% # Removes new workers and unknown workers
    filter(!(partner %in% c("", "new_worker", "unpaintedqueen", "male", "newworker", "?yb"))) %>% # Removes new workers and unknown workers
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
  
    strength <- strength(igraph, mode = "out") # calculate strength
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

############### CHOICE COLONIES ##############
###############    Colony 42    ##############
c42 <- all_data %>% 
  filter(Colony == "n.amb42")

c42_list <- split(c42, c42$Trial) # Creates a list for the three trials

igraphs_42 <- lapply(c42_list, func_igraph) 
igraphs_42 <- lapply(igraphs_42, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_42[[1]])
# coords_42 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_42) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_42[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_42[[2]])
tkplot(igraphs_42[[2]])

tk_set_coords(id, coords_42) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_42[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3
id <- tkplot(igraphs_42[[3]])

tk_set_coords(id, coords_42) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_42[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution


###############    Colony 55    ##############
c55 <- all_data %>% 
       filter(Colony == "n.amb55")

c55_list <- split(c55, c55$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
         filter(colony == 55) %>% 
         select(ID)

igraphs_55 <- lapply(c55_list, func_igraph) 
igraphs_55 <- lapply(igraphs_55, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_55[[1]])
coords_55 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_55) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_55[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_55[[2]])
tkplot(igraphs_55[[2]])

tk_set_coords(id, coords_55) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_55[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3
id <- tkplot(igraphs_55[[3]])

tk_set_coords(id, coords_55) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_55[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution


###############    Colony 41    ##############
c41 <- all_data %>% 
  filter(Colony == "n.amb41")

c41_list <- split(c41, c41$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 41) %>% 
  select(ID)

igraphs_41 <- lapply(c41_list, func_igraph) 
igraphs_41 <- lapply(igraphs_41, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_41[[1]])
coords_41 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_41) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_41[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_41[[2]])

tk_set_coords(id, coords_41) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_41[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3 
id <- tkplot(igraphs_41[[3]])

tk_set_coords(id, coords_41) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_41[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

###############    Colony 53    ##############
c53 <- all_data %>% 
  filter(Colony == "n.amb53")

c53_list <- split(c53, c53$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 53) %>% 
  select(ID)

igraphs_53 <- lapply(c53_list, func_igraph) 
igraphs_53 <- lapply(igraphs_53, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_53[[1]])
coords_53 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_53) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_53[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_53[[2]])

tk_set_coords(id, coords_53) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_53[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3 
id <- tkplot(igraphs_53[[3]])

tk_set_coords(id, coords_53) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_53[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

############### NO-CHOICE COLONIES ##############
###############    Colony 60    ##############
c60 <- all_data %>% 
  filter(Colony == "n.amb60")

c60_list <- split(c60, c60$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 60) %>% 
  select(ID)

igraphs_60 <- lapply(c60_list, func_igraph) 
igraphs_60 <- lapply(igraphs_60, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_60[[1]])
coords_60 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_60) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_60[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_60[[2]])

tk_set_coords(id, coords_60) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_60[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3 
id <- tkplot(igraphs_60[[3]])

tk_set_coords(id, coords_60) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_60[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

###############    Colony 59    ##############
c59 <- all_data %>% 
  filter(Colony == "n.amb59")

c59_list <- split(c59, c59$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 59) %>% 
  select(ID)

igraphs_59 <- lapply(c59_list, func_igraph) 
igraphs_59 <- lapply(igraphs_59, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_59[[1]])
coords_59 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_59) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_59[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_59[[2]])

tk_set_coords(id, coords_59) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_59[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3 
id <- tkplot(igraphs_59[[3]])

tk_set_coords(id, coords_59) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_59[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

###############    Colony 45    ##############
c45 <- all_data %>% 
  filter(Colony == "n.amb45")

c45_list <- split(c45, c45$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 45) %>% 
  select(ID)

igraphs_45 <- lapply(c45_list, func_igraph) 
igraphs_45 <- lapply(igraphs_45, func_plot_igraph) # Creates igraphs for all three trials

# Trial #1
id <- tkplot(igraphs_45[[1]])
coords_45 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_45) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_45[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #2 
id <- tkplot(igraphs_45[[2]])

tk_set_coords(id, coords_45) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_45[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3 
id <- tkplot(igraphs_45[[3]])

tk_set_coords(id, coords_45) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_45[[3]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

###############    Colony 47    ##############
c47 <- all_data %>% 
  filter(Colony == "n.amb47")

c47_list <- split(c47, c47$Trial) # Creates a list for the three trials

nodes <- read.csv("data/node_lists.csv") %>% 
  filter(colony == 47) %>% 
  select(ID)

igraphs_47 <- lapply(c47_list, func_igraph) 
igraphs_47 <- lapply(igraphs_47, func_plot_igraph) # Creates igraphs for all three trials

# Data from trial 1 is missing
# Trial #2
id <- tkplot(igraphs_47[[1]])
coords_47 <- tk_coords(id) # Saves coordinates of trial #1
tk_set_coords(id, coords_47) # Set stored coordinates to trial #1
tkplot.fit.to.screen(id)

hist(V(igraphs_47[[1]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

# Trial #3
id <- tkplot(igraphs_47[[2]])

tk_set_coords(id, coords_47) # Set stored coordinates to trial #2
tkplot.fit.to.screen(id)

hist(V(igraphs_47[[2]])$strength, col = "lightsteelblue1", 
     breaks = c(0:12), xlab = "Out-strength") # Strength distribution

