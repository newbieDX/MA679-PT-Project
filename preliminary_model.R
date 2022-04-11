library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(timeDate)
library(timeSeries)


# Manually extracted GRF and COP features
discrete <- read_excel("/Users/xuchen/Desktop/MA679/PTProject_Data/discrete.xls")
ID_info <- read_excel("/Users/xuchen/Desktop/MA679/PTProject_Data/IDinfo.xls")