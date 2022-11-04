## Intra-class correlations tests 
## Paper: "GesturalOrigins: An ELAN Framework for Establishing Systematic Gesture Data Across Ape Species"
## The datasets are available on github: https://github.com/CharlotteGrund/Gestural_Origins_Coding-methods_paper/tree/main/data  

##### BWINDI (Coders: CG and CH) ##############

# Load packages
library(tidyverse)
library(psych) # Intra-class correlation

# Read Data
CG_bwindi <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_bwindi_data_CG.csv')
CH_bwindi <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_bwindi_data_CH.csv')

## Coder 1 = CG; Coder 2 = CH
xx1 <- CG_bwindi
xx2 <- CH_bwindi

# Calculate durations coder 1
xx1$MAU_duration <- as.numeric(xx1$MAU_end_T) - as.numeric(xx1$G_start_T)
xx1$GA_duration <- as.numeric(xx1$GA_end_T) - as.numeric(xx1$G_start_T)
xx1$MAU_duration <- round(xx1$MAU_duration, digits = 2)
xx1$GA_duration <- round(xx1$GA_duration, digits = 2)

# Calculate durations coder 2
xx2$MAU_duration <- as.numeric(xx2$MAU_end_T) - as.numeric(xx2$G_start_T)
xx2$GA_duration <- as.numeric(xx2$GA_end_T) - as.numeric(xx2$G_start_T)
xx2$MAU_duration <- round(xx2$MAU_duration, digits = 2)
xx2$GA_duration <- round(xx2$GA_duration, digits = 2)

# Calculate the latency for goal to be fulfilled (Coder 1)
xx1$GA.outcome.lat <- as.numeric(xx1$Outcome_T)-as.numeric(xx1$GA_end_T)
xx1$MAU.outcome.lat <- as.numeric(xx1$Outcome_T) - as.numeric(xx1$MAU_end_T)
xx1$GA.outcome.lat <- round(xx1$GA.outcome.lat, digits = 2)
xx1$MAU.outcome.lat <- round(xx1$MAU.outcome.lat, digits = 2)

# Calculate the latency for goal to be fulfilled (Coder 2)
xx2$GA.outcome.lat <- as.numeric(xx2$Outcome_T)-as.numeric(xx2$GA_end_T)
xx2$MAU.outcome.lat <- as.numeric(xx2$Outcome_T) - as.numeric(xx2$MAU_end_T)
xx2$GA.outcome.lat <- round(xx2$GA.outcome.lat, digits = 2)
xx2$MAU.outcome.lat <- round(xx2$MAU.outcome.lat, digits = 2)

## Selection of timings (linked through Rec_number) for ICC (and graphs)
# Coder 1
xx1.ratings <- xx1 %>% select (MAU_duration,
                               GA_duration,
                               GA.outcome.lat, 
                               MAU.outcome.lat, 
                               Rec_number,
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (MAU_duration.xx1 = MAU_duration,
          GA_duration.xx1 = GA_duration,
          GA.outcome.lat.xx1 = GA.outcome.lat, 
          MAU.outcome.lat.xx1 = MAU.outcome.lat,
          Outcome_T.xx1 = Outcome_T,
          GA_end_T.xx1 = GA_end_T,
          MAU_end_T.xx1 = MAU_end_T)

xx2.ratings <- xx2 %>% select (MAU_duration,
                               GA_duration,
                               GA.outcome.lat, 
                               MAU.outcome.lat, 
                               Rec_number,
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (MAU_duration.xx2 = MAU_duration,
          GA_duration.xx2 = GA_duration,
          GA.outcome.lat.xx2 = GA.outcome.lat, 
          MAU.outcome.lat.xx2 = MAU.outcome.lat,
          Outcome_T.xx2 = Outcome_T,
          GA_end_T.xx2 = GA_end_T,
          MAU_end_T.xx2 = MAU_end_T)

## Combined data of Coder 1 and Coder 2
Ratings <- merge(xx1.ratings, xx2.ratings, by = "Rec_number")
Ratings_bwindi <- Ratings
write_csv(Ratings_bwindi,file = "/Users/cvg1/RProjects/Methods paper/Ratings_bwindi.csv")

##### Intra-class correlation tests (Bwindi; Coders: CG and CH)

# MAU duration
MAU.dur.test <- Ratings %>% select(MAU_duration.xx1, MAU_duration.xx2)
ICC(MAU.dur.test, missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

# GA duration
GA.dur.test <- Ratings %>% select(GA_duration.xx1, GA_duration.xx2)
ICC(GA.dur.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

### ICC - MAU Outcome latency
MAU.Outcome.test <- Ratings %>% select(MAU.outcome.lat.xx1, MAU.outcome.lat.xx2)
ICC(MAU.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

### ICC - GA Outcome latency
GA.Outcome.test <- Ratings %>% select(GA.outcome.lat.xx1, GA.outcome.lat.xx2)
ICC(GA.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


##### SONSO (Coders: CG and AS) ##############

# Read Data
CG_sonso <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_sonso_data_CG.csv')
AS_sonso <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_sonso_data_AS.csv')

## Coder 1 = CG; Coder 2 = AS
xx1 <- CG_sonso
xx2 <- AS_sonso

# Calculate the latencies to outcome - Coder 1
xx1$GA.outcome.lat <- as.numeric(xx1$Outcome_T)-as.numeric(xx1$GA_end_T)
xx1$GA.outcome.lat <- round(xx1$GA.outcome.lat, digits = 2)
xx1$MAU.outcome.lat <- as.numeric(xx1$Outcome_T) - as.numeric(xx1$MAU_end_T)
xx1$MAU.outcome.lat <- round(xx1$MAU.outcome.lat, digits = 2)

# Calculate the latencies to outcome - Coder 2
xx2$GA.outcome.lat <- as.numeric(xx2$Outcome_T)-as.numeric(xx2$GA_end_T)
xx2$GA.outcome.lat <- round(xx2$GA.outcome.lat, digits = 2)
xx2$MAU.outcome.lat <- as.numeric(xx2$Outcome_T) - as.numeric(xx2$MAU_end_T)
xx2$MAU.outcome.lat <- round(xx2$MAU.outcome.lat, digits = 2)

## Selection of timings (linked through Rec_number) for ICC (and graphs)
# Coder 1
xx1.ratings <- xx1 %>% select (GA.outcome.lat, 
                               MAU.outcome.lat,
                               Rec_number,
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (GA.outcome.lat.xx1 = GA.outcome.lat, 
          MAU.outcome.lat.xx1 = MAU.outcome.lat,
          Outcome_T.xx1 = Outcome_T,
          GA_end_T.xx1 = GA_end_T,
          MAU_end_T.xx1 = MAU_end_T)

# Coder 2
xx2.ratings <- xx2 %>% select (GA.outcome.lat, 
                               MAU.outcome.lat,
                               Rec_number, 
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (GA.outcome.lat.xx2 = GA.outcome.lat, 
          MAU.outcome.lat.xx2 = MAU.outcome.lat, 
          Outcome_T.xx2 = Outcome_T,
          GA_end_T.xx2 = GA_end_T,
          MAU_end_T.xx2 = MAU_end_T)

## Combined data of Coder 1 and Coder 2
Ratings <- merge(xx1.ratings, xx2.ratings, by = "Rec_number")
Ratings_sonso <- Ratings
write_csv(Ratings_sonso,file = "/Users/cvg1/RProjects/Methods paper/Ratings_sonso.csv")

##### Intra-class correlation tests (SONSO; Coders: CG and AS)

### ICC - MAU Outcome latency
MAU.Outcome.test <- Ratings %>% select(MAU.outcome.lat.xx1, MAU.outcome.lat.xx2)
ICC(MAU.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

### ICC - GA Outcome latency
GA.Outcome.test <- Ratings %>% select(GA.outcome.lat.xx1, GA.outcome.lat.xx2)
ICC(GA.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)


##### WAIBIRA (Coders: CG and GB) ##############

## Read Data
CG_waibira <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_waibira_data_CG.csv')
GB_waibira <- read.csv('/Users/cvg1/Desktop/BRM_MethodsPaper/IRR test/IRR_waibira_data_GB.csv')

## Coder 1 = CG; Coder 2 = GB
xx1 <- CG_waibira
xx2 <- GB_waibira

# Calculate the latencies to outcome - Coder 1
xx1$GA.outcome.lat <- as.numeric(xx1$Outcome_T)-as.numeric(xx1$GA_end_T)
xx1$GA.outcome.lat <- round(xx1$GA.outcome.lat, digits = 2)
xx1$MAU.outcome.lat <- as.numeric(xx1$Outcome_T) - as.numeric(xx1$MAU_end_T)
xx1$MAU.outcome.lat <- round(xx1$MAU.outcome.lat, digits = 2)

# Calculate the latencies to outcome - Coder 2
xx2$GA.outcome.lat <- as.numeric(xx2$Outcome_T)-as.numeric(xx2$GA_end_T)
xx2$GA.outcome.lat <- round(xx2$GA.outcome.lat, digits = 2)
xx2$MAU.outcome.lat <- as.numeric(xx2$Outcome_T) - as.numeric(xx2$MAU_end_T)
xx2$MAU.outcome.lat <- round(xx2$MAU.outcome.lat, digits = 2)

## Selection of timings (linked through Rec_number) for ICC (and graphs)
# Coder 1
xx1.ratings <- xx1 %>% select (GA.outcome.lat, 
                               MAU.outcome.lat,
                               Rec_number,
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (GA.outcome.lat.xx1 = GA.outcome.lat, 
          MAU.outcome.lat.xx1 = MAU.outcome.lat,
          Outcome_T.xx1 = Outcome_T,
          GA_end_T.xx1 = GA_end_T,
          MAU_end_T.xx1 = MAU_end_T)

# Coder 2
xx2.ratings <- xx2 %>% select (GA.outcome.lat, 
                               MAU.outcome.lat,
                               Rec_number, 
                               Outcome_T,
                               GA_end_T,
                               MAU_end_T) %>% 
  rename (GA.outcome.lat.xx2 = GA.outcome.lat, 
          MAU.outcome.lat.xx2 = MAU.outcome.lat, 
          Outcome_T.xx2 = Outcome_T,
          GA_end_T.xx2 = GA_end_T,
          MAU_end_T.xx2 = MAU_end_T)

## Combined data of Coder 1 and Coder 2
Ratings <- merge(xx1.ratings, xx2.ratings, by = "Rec_number")
Ratings_waibira <- Ratings
write_csv(Ratings_waibira,file = "/Users/cvg1/RProjects/Methods paper/Ratings_waibira.csv")


##### Intra-class correlation tests (WAIBIRA; Coders: CG and GB)

### ICC - MAU Outcome latency
MAU.Outcome.test <- Ratings %>% select(MAU.outcome.lat.xx1, MAU.outcome.lat.xx2)
ICC(MAU.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

### ICC - GA Outcome latency
GA.Outcome.test <- Ratings %>% select(GA.outcome.lat.xx1, GA.outcome.lat.xx2)
ICC(GA.Outcome.test,missing=TRUE,alpha=.05,lmer=TRUE,check.keys=FALSE)

