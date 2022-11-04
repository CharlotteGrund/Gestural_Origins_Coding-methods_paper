
## Worked example 1: Difference in variability between the MAU duration, the gesture action duration, and the full gesture duration
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


##### Worked example 1 - Boxplots: MAU, GA, G duration (mountain gorillas) ----------------

# Read data: ESM_worked_example_1_data.csv
data <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/Submission files/Supplementary material/ESM_worked_example_1_data.csv')

# Plot MAU duration
mau.dur.mg <- data %>%
  ggplot( aes(x=Gesture_record, y=MAU_duration, fill=Gesture_record)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=10)
  ) +
  ggtitle("Minimum action unit duration") +
  xlab("")+
  ylab("sec") + 
  coord_cartesian(ylim = c(0, 15))

# Plot GA duration
ga.dur.mg <- data %>%
  ggplot( aes(x=Gesture_record, y=GA_duration, fill=Gesture_record)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=10)
  ) +
  ggtitle("Gesture action duration") +
  xlab("")+
  ylab("sec") + 
  coord_cartesian(ylim = c(0, 15))


# Plot Full Gesture duration
g.dur.mg <- data %>%
  ggplot( aes(x=Gesture_record, y=Gesture_duration, fill=Gesture_record)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=10),
  ) +
  ggtitle("Full gesture duration") +
  xlab("") +
  ylab("sec")+
  coord_cartesian(ylim = c(0, 15))

# putting all in one figure
plot_grid(mau.dur.mg, ga.dur.mg, g.dur.mg, 
          labels = c("A", "B", "C"), label_size = 10,
          ncol = 3, nrow = 1)


# Sample size per gesture action
beckon <- data %>% filter(Gesture_record == "Beckon") #26
raise <- data %>% filter(Gesture_record == "Raise") #54
reach <- data %>% filter(Gesture_record == "Reach") #85


#MAU duration

#median
median(beckon$MAU_duration)
median(raise$MAU_duration)
median(reach$MAU_duration)
#range
range(beckon$MAU_duration)
range(raise$MAU_duration)
range(reach$MAU_duration)
#mean
mean(beckon$MAU_duration)
mean(raise$MAU_duration)
mean(reach$MAU_duration)
#standard deviation
sd(beckon$MAU_duration)
sd(raise$MAU_duration)
sd(reach$MAU_duration)


# GA duration

# median
median(beckon$GA_duration)
median(raise$GA_duration)
median(reach$GA_duration)
# range
range(beckon$GA_duration)
range(raise$GA_duration)
range(reach$GA_duration)
# mean
mean(beckon$GA_duration)
mean(raise$GA_duration)
mean(reach$GA_duration)
# standard deviation
sd(beckon$GA_duration)
sd(raise$GA_duration)
sd(reach$GA_duration)


# full gesture duration

# median
median(beckon$Gesture_duration)
median(raise$Gesture_duration)
median(reach$Gesture_duration)
# range
range(beckon$Gesture_duration)
range(raise$Gesture_duration)
range(reach$Gesture_duration)
# mean
mean(beckon$Gesture_duration)
mean(raise$Gesture_duration)
mean(reach$Gesture_duration)
# standard deviation
sd(beckon$Gesture_duration)
sd(raise$Gesture_duration)
sd(reach$Gesture_duration)

