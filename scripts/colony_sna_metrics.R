## setwd("C:/Users/jy33/OneDrive/Desktop/R/ant_sna")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)

## Load and clean data
sna_metrics <- read.csv("data/colony_sna_metrics.csv")

# Plot change in density over successive emigrations
ggplot(data = sna_metrics, aes(y = tie_density, x = emigration, color = treatment)) + 
       geom_point(size = 2) + geom_smooth(method = "lm") + 
       scale_x_continuous(name = "Emigration", breaks = c(1, 3, 5)) + 
       scale_y_continuous(name = "Density", breaks = c(0, 0.01, 0.015, 0.02, 0.025))

# Density model
density_model <- lmer(data = sna_metrics, tie_density ~ emigration*treatment + 
                     (1|colony))

plot(simulateResiduals(density_model)) # Diagnositc plots
summary(density_model) 
Anova(density_model)










