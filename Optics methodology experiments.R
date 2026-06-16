## Script for testing various spectral processing procedures and their effects on vision model output

#Base code
# this clears all previous data in the R session
rm(list=ls());gc()
dev.off()
#load packages
library(tidyverse)
library(pavo)
library(egg)

#Set working directory and load the dataset
setwd("C:/Users/P309883/OneDrive - University of Groningen/Desktop/Double deception study/Main data")
data<- read.csv("UV smoothed.csv", sep=";") %>% select(contains("Anemone")) %>% as.rspec() 
data$wl<- c(300:800)

#inspect data
plot(data)
