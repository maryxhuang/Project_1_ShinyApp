library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapproj)
library(datasets)
#library(reshape2)
library(stringr)
library(RColorBrewer)

#setwd("E:/E-Documents/NYC_Data_Science_Academy/Project_1_ShinyApp/v8_0724")
ushr <- read.csv('./homicide-reports.csv', stringsAsFactors=F)
ushr <- ushr %>% 
  select(., State, Year, Crime.Solved, Victim.Sex, Victim.Age, 
         Perpetrator.Sex, Perpetrator.Age, Relationship, Weapon)

# convert matrix to dataframe
# state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
# rownames(state_stat) <- NULL
# create variable with colnames as choice
# choice <- colnames(state_stat)[-1]