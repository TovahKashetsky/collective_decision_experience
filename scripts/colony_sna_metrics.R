## setwd("C:/Users/jy33/OneDrive/Desktop/R/ant_sna")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)


## Load and clean data
sna_metrics <- read.csv("data/colony_sna_metrics.csv")
sna_metrics$colony <- as.factor(sna_metrics$colony)

# Plot change in density over successive emigrations
ggplot(data = sna_metrics, aes(y = tie_density, x = emigration, color = colony)) + 
       geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) + 
       scale_x_continuous(name = "Emigration", breaks = c(1, 3, 5)) + 
       ylim(0, 0.025) + ylab("Density") +
       facet_grid(~treatment)

# Density model
density_model <- lmer(data = sna_metrics, tie_density ~ emigration*treatment + 
                     (1|colony))

plot(simulateResiduals(density_model)) # Diagnostic plots
summary(density_model)
Anova(density_model)

# Plot change in coef of variation over successive emigrations
ggplot(data = sna_metrics, aes(y = coefficient_of_variation, x = emigration, color = colony)) + 
       geom_point(size = 2) + geom_smooth(method = "lm", se = FALSE) +
       scale_x_continuous(name = "Emigration", breaks = c(1, 3, 5)) + 
       ylim(1, 3.5) + ylab("Outstrength coefficient of variation") + 
       facet_grid(~treatment) + scale_color_nejm()

# CoV model
cov_model <- lmer(data = sna_metrics, coefficient_of_variation ~ emigration*treatment + 
                        (1|colony))

plot(simulateResiduals(cov_model)) # Diagnostic plots
summary(cov_model)
Anova(cov_model)






