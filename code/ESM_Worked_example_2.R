
## Worked example 2: Comparison of recipient response latencies in EAC and MG (MAU.Outcome latency; GA.Outcome latency)
## Paper: "GesturalOrigins: An ELAN Framework for Establishing Systematic Gesture Data Across Ape Species"
## The datasets are available on github: https://github.com/CharlotteGrund/Gestural_Origins_Coding-methods_paper/tree/main/data  

# clear environment/load packages
rm(list=ls())
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(ggpubr)
library("cowplot")


## Worked example 2.1 - MAU.Outcome latency; GA.Outcome latency comparison for presents for grooming ####

# Read data: ESM_worked_example_1_data.csv
data <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/Submission files/Supplementary material/ESM_worked_example_2.1_data.csv')

# range of MAU.Outcome latency for EAC
xx <- data %>% filter(Species == "EAC") 
range(xx$LatencyMAU.goal)
# range of MAU.Outcome latency for MG
xx <- data %>% filter(Species == "MG") 
range(xx$LatencyMAU.goal)
# range of GA.Outcome latency for EAC
xx <- data %>% filter(Species == "EAC") 
range(xx$LatencyGA.goal)
# range of GA.Outcome latency for MG
xx <- data %>% filter(Species == "MG") 
range(xx$LatencyGA.goal)

# median of MAU.Outcome latency for EAC
xx <- data %>% filter(Species == "EAC") 
median(xx$LatencyMAU.goal)
# median of MAU.Outcome latency for MG
xx <- data %>% filter(Species == "MG") 
median(xx$LatencyMAU.goal)

# mean of MAU.Outcome latency for EAC
xx <- data %>% filter(Species == "EAC") 
mean(xx$LatencyMAU.goal)
# range of MAU.Outcome latency for MG
xx <- data %>% filter(Species == "MG") 
mean(xx$LatencyMAU.goal)

# standard deviation of MAU.Outcome latency for EAC
xx <- data %>% filter(Species == "EAC") 
sd(xx$LatencyMAU.goal)
# range of MAU.Outcome latency for MG
xx <- data %>% filter(Species == "MG") 
sd(xx$LatencyMAU.goal)


# Boxplot GA.Outcome latency (presents for grooming)
latency.ga.prs <- data %>%
  ggplot( aes(x=Species, y=LatencyGA.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Latency from GA end to Outcome (for 'Present')") +
  xlab("")+
  ylab("(sec)") +
  coord_cartesian(ylim = c(-2, 45))


# Boxplot MAU.Outcome latency (presents for grooming)
latency.mau.prs <- data %>%
  ggplot( aes(x=Species, y=LatencyMAU.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Latency from MAU end to Outcome (for 'Present')") +
  xlab("") +
  ylab("(sec)")+
  coord_cartesian(ylim = c(-2, 45))

plot_grid(latency.mau.prs,latency.ga.prs,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)



## Worked example 2.2 - MAU.Outcome latency; GA.Outcome latency comparison for all gesture actions (and all goals except play) ####

# Read data: ESM_worked_example_2_data.csv
data <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/Submission files/Supplementary material/ESM_worked_example_2.2_data.csv')

# range, median, mean, sd ## GA.Outcome latency
round(range(data$LatencyGA.goal), digits = 2)
round(median(data$LatencyGA.goal), digits = 2)
round(mean(data$LatencyGA.goal), digits = 2)
round(sd(data$LatencyGA.goal), digits = 2)
# range, median, mean, sd ## MAU.Outcome latency
round(range(data$LatencyMAU.goal), digits = 2)
round(median(data$LatencyMAU.goal), digits = 2)
round(mean(data$LatencyMAU.goal), digits = 2)
round(sd(data$LatencyMAU.goal), digits = 2)

# range, median, mean, sd ## GA.Outcome latency (Species == EAC)
xx <- data %>% filter(Species == "EAC") 
round(range(xx$LatencyGA.goal), digits = 2)
round(median(xx$LatencyGA.goal), digits = 2)
round(mean(xx$LatencyGA.goal), digits = 2)
round(sd(xx$LatencyGA.goal), digits = 2)
# range, median, mean, sd ## MAU.Outcome latency (Species == MG)
xx <- data %>% filter(Species == "MG") 
round(range(xx$LatencyMAU.goal), digits = 2)
round(median(xx$LatencyMAU.goal), digits = 2)
round(mean(xx$LatencyMAU.goal), digits = 2)
round(sd(xx$LatencyMAU.goal), digits = 2)


# Boxplot MAU.Outcome latency
latency.mau.all.1 <- data %>%
  ggplot( aes(x=Species, y=LatencyMAU.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from MAU end to outcome") +
  xlab("") +
  ylab("(sec)")+
  coord_cartesian(ylim = c(-10, 80))

# Boxplot GA.Outcome latency
latency.ga.all.1 <- data %>%
  ggplot( aes(x=Species, y=LatencyGA.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from GA end to outcome") +
  xlab("")+
  ylab("(sec)") +
  coord_cartesian(ylim = c(-10, 80))


# Boxplot MAU.Outcome latency - zoomed in
latency.mau.all.2 <- data %>%
  ggplot( aes(x=Species, y=LatencyMAU.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from MAU end to outcome") +
  xlab("") +
  ylab("(sec)")+
  coord_cartesian(ylim = c(-5, 25))

# Boxplot GA.Outcome latency - zoomed in
latency.ga.all.2 <- data %>%
  ggplot( aes(x=Species, y=LatencyGA.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from GA end to outcome") +
  xlab("")+
  ylab("(sec)") +
  coord_cartesian(ylim = c(-5, 25))


plot_grid(latency.mau.all.1, latency.ga.all.1, latency.mau.all.2,latency.ga.all.2,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)



