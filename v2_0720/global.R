library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapproj)
library(datasets)

setwd("E:/E-Documents/NYC_Data_Science_Academy/Project_1_ShinyApp")
ushr <- read.csv('./hr_cut.csv', stringsAsFactors=F)

# convert matrix to dataframe
# state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
# rownames(state_stat) <- NULL
# create variable with colnames as choice
# choice <- colnames(state_stat)[-1]