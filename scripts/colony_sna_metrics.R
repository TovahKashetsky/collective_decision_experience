## setwd("C:/Users/jy33/OneDrive/Desktop/R/ant_sna")

library(tidyverse)
library(ggplot2); theme_set(theme_classic())
library(lme4)
library(DHARMa)
library(car)
library(ggsci)
library(ggthemes)
library(RColorBrewer)

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16), 
  axis.text.y = element_text(size = 16))


## Load and clean data
sna_metrics <- read.csv("data/colony_sna_metrics.csv")
sna_metrics$colony <- as.factor(sna_metrics$colony)

# Plot change in density over successive emigrations
ggplot(data = sna_metrics, aes(y = tie_density, x = emigration, color = colony)) + 
       geom_point(size = 3, alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, size = 1.5) + 
       scale_x_continuous(name = "Emigration", breaks = c(1, 3, 5)) + 
       ylim(0, 0.03) + ylab("Density") + theme_base() +
       facet_grid(~treatment) + 
       scale_color_manual(values = c("#B6440B", "#e6550d", "#fdae6b", "#FCC792", "#175230", "#23804B", "#43B198", "#99d8c9"))

# Density model
density_model <- lmer(data = sna_metrics, tie_density ~ emigration*treatment + 
                     (1|colony))

plot(simulateResiduals(density_model)) # Diagnostic plots
summary(density_model)
Anova(density_model)

# Plot change in coef of variation over successive emigrations
sna_metrics$treatment <- recode_factor(sna_metrics$treatment, choice = "Choice", no_choice = "No choice")

sna_metrics$colony <- factor(sna_metrics$colony, levels = c("41", "42", "53", "55", "45", "47", "59", "60"))

ggplot(data = sna_metrics, aes(y = coefficient_of_variation, x = emigration, color = colony)) + 
       geom_point(size = 3, alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, size = 1.5) +
       scale_x_continuous(name = "Emigration", breaks = c(1, 3, 5)) + 
       ylim(1, 3.5) + ylab("Outstrength coefficient of variation") + 
       facet_grid(~treatment) + theme_base() + 
       scale_color_manual(values = c("#B6440B", "#e6550d", "#fdae6b", "#FCC792", "#175230", "#23804B", "#43B198", "#99d8c9"))


# CoV model
cov_model <- lmer(data = sna_metrics, coefficient_of_variation ~ emigration*treatment + 
                        (1|colony))

plot(simulateResiduals(cov_model)) # Diagnostic plots
summary(cov_model)
Anova(cov_model)






