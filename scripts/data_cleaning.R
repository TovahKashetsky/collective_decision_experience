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
     filter(behaviour %in% c("transport", "tandem run", "reverse tandem run")) %>% # Removes trial starts and discoveries
     filter(successful != "failed") %>% # Removes failed TRs and RTRs 
     filter(focal != "" & focal != "new_worker") %>% # Removes new workers and unknown workers
     filter(partner != "" & partner != "new_worker") %>% # Removes new workers and unknown workers
     filter(!(origin == "dark" & destination == "dim")) %>%  # Removes indirect interactions
     filter(!(origin == "dim" & destination == "dark")) %>% # Removes indirect interactions
     filter(!(origin == "dim" & destination == "dim")) %>% # Removes indirect interactions
     filter(!(origin == "dark" & destination == "dark")) %>% # Removes indirect interactions
     select(Group, Colony, Trial, behaviour, focal, partner)


trial_list <- split(n42_data, n42_data$Trial)