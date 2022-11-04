
## IRR supplementary material
## Paper: "GesturalOrigins: An ELAN Framework for Establishing Systematic Gesture Data Across Ape Species"
## The data are available on github: https://github.com/CharlotteGrund/Gestural_Origins_Coding-methods_paper/tree/main/data  


## Load packages
rm(list=ls())
library(tidyverse)


##### BWINDI (Coders: CG and CH) ##############

# Read Data
Ratings_bwindi <- read.csv('/Users/cvg1/RProjects/Methods paper/Ratings_bwindi.csv')
Ratings <- Ratings_bwindi

# Mean ratings
Ratings$mean_GA.outcome.lat <- (Ratings$GA.outcome.lat.xx1 + Ratings$GA.outcome.lat.xx2)/2
Ratings$mean_MAU.outcome.lat <- (Ratings$MAU.outcome.lat.xx1 + Ratings$MAU.outcome.lat.xx2)/2
Ratings$mean_MAU_end <- (Ratings$MAU_end_T.xx1 + Ratings$MAU_end_T.xx2)/2
Ratings$mean_GA_end <- (Ratings$GA_end_T.xx1 + Ratings$GA_end_T.xx2)/2
Ratings$mean_Outcome <- (Ratings$Outcome_T.xx1 + Ratings$Outcome_T.xx2)/2
Ratings$mean_MAU_dur <- (Ratings$MAU_duration.xx1 + Ratings$MAU_duration.xx2)/2
Ratings$mean_GA_dur <- (Ratings$GA_duration.xx1 + Ratings$GA_duration.xx2)/2

# Coder 1 deviation (from the mean)
Ratings$Coder1_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx1
Ratings$Coder1_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx1
Ratings$Coder1_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx1
Ratings$Coder1_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx1
Ratings$Coder1_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx1
Ratings$Coder1_MAU_dur_div <- Ratings$mean_MAU_dur - Ratings$MAU_duration.xx1
Ratings$Coder1_GA_dur_div <- Ratings$mean_GA_dur - Ratings$GA_duration.xx1

#Coder 2 deviation (from the mean)
Ratings$Coder2_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx2
Ratings$Coder2_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx2
Ratings$Coder2_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx2
Ratings$Coder2_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx2
Ratings$Coder2_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx2
Ratings$Coder2_MAU_dur_div <- Ratings$mean_MAU_dur - Ratings$MAU_duration.xx2
Ratings$Coder2_GA_dur_div <- Ratings$mean_GA_dur - Ratings$GA_duration.xx2

# combining data for visualisation
x1 <- Ratings %>% select(Coder1_GA.outcome.lat_div, Coder1_GA_div, 
                         Coder1_MAU.outcome.lat_div, Coder1_MAU_div, 
                         Coder1_Outcome_div, Coder1_MAU_dur_div, Coder1_GA_dur_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder1_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder1_MAU.outcome.lat_div, 
         GA_div = Coder1_GA_div, 
         MAU_div = Coder1_MAU_div,
         Outcome_div = Coder1_Outcome_div,
         MAU_dur_div = Coder1_MAU_dur_div,
         GA_dur_div = Coder1_GA_dur_div) %>% 
  arrange(Rec_number)
x1$Coder <- "CG"
x1$Number <- 1:40


x2 <- Ratings %>% select(Coder2_GA.outcome.lat_div, Coder2_GA_div, 
                         Coder2_MAU.outcome.lat_div, Coder2_MAU_div, 
                         Coder2_Outcome_div,Coder2_MAU_dur_div, Coder2_GA_dur_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder2_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder2_MAU.outcome.lat_div, 
         GA_div = Coder2_GA_div, 
         MAU_div = Coder2_MAU_div,
         Outcome_div = Coder2_Outcome_div,
         MAU_dur_div = Coder2_MAU_dur_div,
         GA_dur_div = Coder2_GA_dur_div) %>% 
  arrange(Rec_number)
x2$Coder <- "CH"
x2$Number <- 1:40

xx <- rbind(x1,x2)


## Figure ESM.1: Inter-rater agreement between the coders CG and CH (Species: mountain gorillas) 
# Plots of deviation from mean between the coders

xlatmau <- ggplot(xx, aes(y=MAU.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU.Outcome latency") +ylim(-1.5, 1.5)

xlatga <- ggplot(xx, aes(y=GA.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA.Outcome latency") +ylim(-1.5, 1.5)

xoutcome <- ggplot(xx, aes(y=Outcome_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="Outcome time measurement") +ylim(-1.5, 1.5)

xmaudur <- ggplot(xx, aes(y=MAU_dur_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU duration measurement") +ylim(-1.5, 1.5)

xgadur <- ggplot(xx, aes(y=GA_dur_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA duration measurement") +ylim(-1.5, 1.5)


library("cowplot")
plot_grid(xmaudur, xgadur, xoutcome, xlatmau, xlatga, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

## Deviation in coding time variables

# Variable 1: MAU duration
MAU.dur <- Ratings %>% select(MAU_duration.xx1, MAU_duration.xx2)
yy <- MAU.dur
# coder rating deviation
yy$Difference <- yy$MAU_duration.xx1 - yy$MAU_duration.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU_duration.xx1)
mean_xx2 <- mean(yy$MAU_duration.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 2: GA duration
GA.dur <- Ratings %>% select(GA_duration.xx1, GA_duration.xx2)
yy <- GA.dur
# coder rating deviation
yy$Difference <- yy$GA_duration.xx1 - yy$GA_duration.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA_duration.xx1)
mean_xx2 <- mean(yy$GA_duration.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 3: MAU end T
MAU.end.T <- Ratings %>% select(MAU_end_T.xx1, MAU_end_T.xx2)
yy <- MAU.end.T
# coder rating deviation
yy$Difference <- yy$MAU_end_T.xx1 - yy$MAU_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU_end_T.xx1)
mean_xx2 <- mean(yy$MAU_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 4: GA end T
GA.end.T <- Ratings %>% select(GA_end_T.xx1, GA_end_T.xx2)
yy <- GA.end.T
# coder rating deviation
yy$Difference <- yy$GA_end_T.xx1 - yy$GA_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA_end_T.xx1)
mean_xx2 <- mean(yy$GA_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 5: Outcome T
Outcome.T <- Ratings %>% select(Outcome_T.xx1, Outcome_T.xx2)
yy <- Outcome.T
# coder rating deviation
yy$Difference <- yy$Outcome_T.xx1 - yy$Outcome_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$Outcome_T.xx1)
mean_xx2 <- mean(yy$Outcome_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)




##### SONSO (Coders: CG and AS) ##############

# Read Data
Ratings_sonso <- read.csv('/Users/cvg1/RProjects/Methods paper/Ratings_sonso.csv')
Ratings <- Ratings_sonso

# Mean ratings
Ratings$mean_GA.outcome.lat <- (Ratings$GA.outcome.lat.xx1 + Ratings$GA.outcome.lat.xx2)/2
Ratings$mean_MAU.outcome.lat <- (Ratings$MAU.outcome.lat.xx1 + Ratings$MAU.outcome.lat.xx2)/2
Ratings$mean_MAU_end <- (Ratings$MAU_end_T.xx1 + Ratings$MAU_end_T.xx2)/2
Ratings$mean_GA_end <- (Ratings$GA_end_T.xx1 + Ratings$GA_end_T.xx2)/2
Ratings$mean_Outcome <- (Ratings$Outcome_T.xx1 + Ratings$Outcome_T.xx2)/2

#Coder 1 deviation (from the mean)
Ratings$Coder1_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx1
Ratings$Coder1_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx1
Ratings$Coder1_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx1
Ratings$Coder1_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx1
Ratings$Coder1_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx1

#Coder 2 deviation (from the mean)
Ratings$Coder2_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx2
Ratings$Coder2_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx2
Ratings$Coder2_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx2
Ratings$Coder2_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx2
Ratings$Coder2_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx2

# combining data for visualisation
x1 <- Ratings %>% select(Coder1_GA.outcome.lat_div, Coder1_GA_div, 
                         Coder1_MAU.outcome.lat_div, Coder1_MAU_div, 
                         Coder1_Outcome_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder1_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder1_MAU.outcome.lat_div, 
         GA_div = Coder1_GA_div, 
         MAU_div = Coder1_MAU_div,
         Outcome_div = Coder1_Outcome_div) %>% 
  arrange(Rec_number)
x1$Coder <- "CG"
x1$Number <- 1:25

x2 <- Ratings %>% select(Coder2_GA.outcome.lat_div, Coder2_GA_div, 
                         Coder2_MAU.outcome.lat_div, Coder2_MAU_div, 
                         Coder2_Outcome_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder2_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder2_MAU.outcome.lat_div, 
         GA_div = Coder2_GA_div, 
         MAU_div = Coder2_MAU_div,
         Outcome_div = Coder2_Outcome_div) %>% 
  arrange(Rec_number)
x2$Coder <- "AS"
x2$Number <- 1:25

xx <- rbind(x1,x2)


## Figure ESM.2: Inter-rater agreement between the coders CG and AS (Species: EAC, Sonso community) 
# Plots of deviation from mean between the coders

xmau <- ggplot(xx, aes(y=MAU_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU time measurement") +ylim(-2, 2)

xga <- ggplot(xx, aes(y=GA_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA time measurement") +ylim(-2, 2)

xlatmau <- ggplot(xx, aes(y=MAU.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU.Outcome latency measurement") +ylim(-2, 2)

xlatga <- ggplot(xx, aes(y=GA.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA.Outcome latency measurement") +ylim(-2, 2)

xoutcome <- ggplot(xx, aes(y=Outcome_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="Outcome time measurement") +ylim(-2, 2)

library("cowplot")
plot_grid(xmau, xga, xlatmau, xlatga, xoutcome, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

## Deviation in coding time variables

# Variable 1: MAU Outcome latency
MAU.Outcome.lat <- Ratings %>% select(MAU.outcome.lat.xx1, MAU.outcome.lat.xx2)
yy <- MAU.Outcome.lat
# coder rating deviation
yy$Difference <- yy$MAU.outcome.lat.xx1 - yy$MAU.outcome.lat.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU.outcome.lat.xx1)
mean_xx2 <- mean(yy$MAU.outcome.lat.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 2: GA Outcome latency
GA.Outcome.lat <- Ratings %>% select(GA.outcome.lat.xx1, GA.outcome.lat.xx2)
yy <- GA.Outcome.lat
# coder rating deviation
yy$Difference <- yy$GA.outcome.lat.xx1 - yy$GA.outcome.lat.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA.outcome.lat.xx1)
mean_xx2 <- mean(yy$GA.outcome.lat.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 3: MAU end T
MAU.end.T <- Ratings %>% select(MAU_end_T.xx1, MAU_end_T.xx2)
yy <- MAU.end.T
# coder rating deviation
yy$Difference <- yy$MAU_end_T.xx1 - yy$MAU_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU_end_T.xx1)
mean_xx2 <- mean(yy$MAU_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 4: GA end T
GA.end.T <- Ratings %>% select(GA_end_T.xx1, GA_end_T.xx2)
yy <- GA.end.T
# coder rating deviation
yy$Difference <- yy$GA_end_T.xx1 - yy$GA_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA_end_T.xx1)
mean_xx2 <- mean(yy$GA_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 5: Outcome T
Outcome.T <- Ratings %>% select(Outcome_T.xx1, Outcome_T.xx2)
yy <- Outcome.T
# coder rating deviation
yy$Difference <- yy$Outcome_T.xx1 - yy$Outcome_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$Outcome_T.xx1)
mean_xx2 <- mean(yy$Outcome_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)




##### WAIBIRA (Coders: CG and GB) ##############

## Read Data
Ratings_waibira <- read.csv('/Users/cvg1/RProjects/Methods paper/Ratings_waibira.csv')
Ratings <- Ratings_waibira

# Mean ratings
Ratings$mean_GA.outcome.lat <- (Ratings$GA.outcome.lat.xx1 + Ratings$GA.outcome.lat.xx2)/2
Ratings$mean_MAU.outcome.lat <- (Ratings$MAU.outcome.lat.xx1 + Ratings$MAU.outcome.lat.xx2)/2
Ratings$mean_MAU_end <- (Ratings$MAU_end_T.xx1 + Ratings$MAU_end_T.xx2)/2
Ratings$mean_GA_end <- (Ratings$GA_end_T.xx1 + Ratings$GA_end_T.xx2)/2
Ratings$mean_Outcome <- (Ratings$Outcome_T.xx1 + Ratings$Outcome_T.xx2)/2

#Coder 1 deviation (from the mean)
Ratings$Coder1_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx1
Ratings$Coder1_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx1
Ratings$Coder1_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx1
Ratings$Coder1_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx1
Ratings$Coder1_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx1

#Coder 2 deviation (from the mean)
Ratings$Coder2_GA.outcome.lat_div <- Ratings$mean_GA.outcome.lat - Ratings$GA.outcome.lat.xx2
Ratings$Coder2_MAU.outcome.lat_div <- Ratings$mean_MAU.outcome.lat - Ratings$MAU.outcome.lat.xx2
Ratings$Coder2_MAU_div <- Ratings$mean_MAU_end - Ratings$MAU_end_T.xx2
Ratings$Coder2_GA_div <- Ratings$mean_GA_end - Ratings$GA_end_T.xx2
Ratings$Coder2_Outcome_div <- Ratings$mean_Outcome - Ratings$Outcome_T.xx2

# combining data for visualisation
x1 <- Ratings %>% select(Coder1_GA.outcome.lat_div, Coder1_GA_div, 
                         Coder1_MAU.outcome.lat_div, Coder1_MAU_div, 
                         Coder1_Outcome_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder1_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder1_MAU.outcome.lat_div, 
         GA_div = Coder1_GA_div, 
         MAU_div = Coder1_MAU_div,
         Outcome_div = Coder1_Outcome_div) %>% 
  arrange(Rec_number)
x1$Coder <- "CG"
x1$Number <- 1:20

x2 <- Ratings %>% select(Coder2_GA.outcome.lat_div, Coder2_GA_div, 
                         Coder2_MAU.outcome.lat_div, Coder2_MAU_div, 
                         Coder2_Outcome_div,
                         Rec_number) %>% 
  rename(GA.outcome.lat_div = Coder2_GA.outcome.lat_div, 
         MAU.outcome.lat_div = Coder2_MAU.outcome.lat_div, 
         GA_div = Coder2_GA_div, 
         MAU_div = Coder2_MAU_div,
         Outcome_div = Coder2_Outcome_div) %>% 
  arrange(Rec_number)
x2$Coder <- "GB"
x2$Number <- 1:20

xx <- rbind(x1,x2)


## Figure ESM.3: Inter-rater agreement between the coders CG and GB (Species: EAC, Waibira community) 
# Plots of deviation from mean between the coders

xmau <- ggplot(xx, aes(y=MAU_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU time measurement") +ylim(-1, 1)

xga <- ggplot(xx, aes(y=GA_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA time measurement") +ylim(-1, 1)

xlatmau <- ggplot(xx, aes(y=MAU.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="MAU.Outcome latency measurement") +ylim(-1, 1)

xlatga <- ggplot(xx, aes(y=GA.outcome.lat_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="GA.Outcome latency measurement") +ylim(-1, 1)

xoutcome <- ggplot(xx, aes(y=Outcome_div, x=Number, shape=Coder, color=Coder)) +
  geom_point(size=2, shape=23) + ylab(label="deviation from mean (sec)") + 
  xlab(label="Outcome time measurement") +ylim(-1, 1)

library("cowplot")
plot_grid(xmau, xga, xlatmau, xlatga, xoutcome, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

## Deviation in coding time variables

# Variable 1: MAU Outcome latency
MAU.Outcome.lat <- Ratings %>% select(MAU.outcome.lat.xx1, MAU.outcome.lat.xx2)
yy <- MAU.Outcome.lat
# coder rating deviation
yy$Difference <- yy$MAU.outcome.lat.xx1 - yy$MAU.outcome.lat.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU.outcome.lat.xx1)
mean_xx2 <- mean(yy$MAU.outcome.lat.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 2: GA Outcome latency
GA.Outcome.lat <- Ratings %>% select(GA.outcome.lat.xx1, GA.outcome.lat.xx2)
yy <- GA.Outcome.lat
# coder rating deviation
yy$Difference <- yy$GA.outcome.lat.xx1 - yy$GA.outcome.lat.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA.outcome.lat.xx1)
mean_xx2 <- mean(yy$GA.outcome.lat.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 3: MAU end T
MAU.end.T <- Ratings %>% select(MAU_end_T.xx1, MAU_end_T.xx2)
yy <- MAU.end.T
# coder rating deviation
yy$Difference <- yy$MAU_end_T.xx1 - yy$MAU_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$MAU_end_T.xx1)
mean_xx2 <- mean(yy$MAU_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 4: GA end T
GA.end.T <- Ratings %>% select(GA_end_T.xx1, GA_end_T.xx2)
yy <- GA.end.T
# coder rating deviation
yy$Difference <- yy$GA_end_T.xx1 - yy$GA_end_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$GA_end_T.xx1)
mean_xx2 <- mean(yy$GA_end_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)

# Variable 5: Outcome T
Outcome.T <- Ratings %>% select(Outcome_T.xx1, Outcome_T.xx2)
yy <- Outcome.T
# coder rating deviation
yy$Difference <- yy$Outcome_T.xx1 - yy$Outcome_T.xx2
# coder rating mean
mean_xx1 <- mean(yy$Outcome_T.xx1)
mean_xx2 <- mean(yy$Outcome_T.xx2)
# mean deviation between coders 
mean_diff <- round(mean(yy$Difference), digits = 2)
# mean absolute deviation between coders 
yy$Difference_pos <- abs(yy$Difference)
mean_diff_pos <- round(mean(yy$Difference_pos), digits = 2)




# Figure ESM.4: Replication of worked example 2.2 
# (“Mountain gorilla and chimpanzee recipient latencies to (behaviourally) respond to a signaller’s 
# gestures..”) - accounting for coding deviation between AS and CG

# Adjusting AS data (adding mean deviation between AS and CG (see IRR) to AS latencies)

# Packages
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library("cowplot")

# Read data
action.data <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/Submission files/Supplementary material/ESM_worked_example_2.2_data.csv')

# Adding the mean deviation in coding MAU.Outcome latency between CG and AS to AS latency values 
action.data.adj <- action.data
action.data.adj$LatencyMAU.goal <-
  ifelse(action.data.adj$Social_unit == 'Sonso',
         (action.data.adj$LatencyMAU.goal+0.48),
         action.data.adj$LatencyMAU.goal)

# Adding the mean deviation in coding GA.Outcome latency between CG and AS to AS latency values
action.data.adj$LatencyGA.goal <-
  ifelse(action.data.adj$Social_unit == 'Sonso',
         (action.data.adj$LatencyGA.goal+0.38),
         action.data.adj$LatencyGA.goal)

# Plot latency from MAU
latency.mau.all.1 <- action.data %>%
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
  coord_cartesian(ylim = c(-5, 10))

latency.mau.all.2 <- action.data.adj %>%
  ggplot( aes(x=Species, y=LatencyMAU.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from MAU end to outcome - adjusted") +
  xlab("")+
  ylab("(sec)") +
  coord_cartesian(ylim = c(-5, 10))


# Plot latency from GA

latency.ga.all.1 <- action.data %>%
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
  xlab("") +
  ylab("(sec)")+
  coord_cartesian(ylim = c(-5, 10))

latency.ga.all.2 <- action.data.adj %>%
  ggplot( aes(x=Species, y=LatencyGA.goal, fill=Species)) +
  geom_boxplot(bins=30) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="grey", size=0.2, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9)
  ) +
  ggtitle("Latency from GA end to outcome - adjusted") +
  xlab("")+
  ylab("(sec)") +
  coord_cartesian(ylim = c(-5, 10))

plot_grid(latency.mau.all.1, latency.mau.all.2, latency.ga.all.1,latency.ga.all.2,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


