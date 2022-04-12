library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(timeDate)
library(timeSeries)


# Manually extracted GRF and COP features
discrete <- read_excel("/Users/xuchen/Desktop/MA679/PTProject_Data/discrete.xls")
ID_info <- read_excel("/Users/xuchen/Desktop/MA679/PTProject_Data/IDinfo.xls")


# Time series data
GRFx <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/GRFx.csv", header = F)
GRFy <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/GRFy.csv", header = F)
GRFz <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/GRFz.csv", header = F)
COPy <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/COPy.csv", header = F)
COPx <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/COPx.csv", header = F)
Mx <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/Mx.csv", header = F)
My <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/My.csv", header = F)
Mz <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/Mz.csv", header = F)

# rbind ID_info and discrete

ID_discrete <- cbind(ID_info, discrete)
ID_GRFx <- cbind(ID_info, GRFx)
ID_GRFy <- cbind(ID_info, GRFy)
ID_GRFz <- cbind(ID_info, GRFz)
ID_Mx <- cbind(ID_info, Mx)
ID_My <- cbind(ID_info, My)
ID_Mz <- cbind(ID_info, Mz)


# plot:

wrangle_data_func <- function(df){
  df1 <- df %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "force")
  df1 <- df1 %>% mutate(new = paste(ID, "-",KNEE, "-",TRIAL))
  df1$data_point <- gsub('^.', '', df1$data_point)
  return(df1)
}

df_COPx <- wrangle_data_func(head(ID_COPx, 250))
df_COPy <- wrangle_data_func(head(ID_COPy, 250))
df_GRFx <- wrangle_data_func(head(ID_GRFx, 250))
df_GRFy <- wrangle_data_func(head(ID_GRFy, 250))
df_GRFz <- wrangle_data_func(head(ID_GRFz, 250))
df_Mx <- wrangle_data_func(head(ID_Mx, 250))
df_My <- wrangle_data_func(head(ID_My, 250))
df_Mz <- wrangle_data_func(head(ID_Mz, 250))



tr_left_2_COPx <- subset(df_COPx, df_COPx$new == "2 - LEFT - 2")
tr_left_2_COPy <- subset(df_COPy, df_COPy$new == "2 - LEFT - 2")
tr_left_2_GRFx <- subset(df_GRFx, df_GRFx$new == "2 - LEFT - 2")
tr_left_2_GRFy <- subset(df_GRFy, df_GRFy$new == "2 - LEFT - 2")
tr_left_2_GRFz <- subset(df_GRFz, df_GRFz$new == "2 - LEFT - 2")
tr_left_2_Mx <- subset(df_Mx, df_Mx$new == "2 - LEFT - 2")
tr_left_2_My <- subset(df_My, df_My$new == "2 - LEFT - 2")
tr_left_2_Mz <- subset(df_Mz, df_Mz$new == "2 - LEFT - 2")

#COPx and COPy
plot(x = tr_left_2_COPx$data_point, y = tr_left_2_COPx$force, type = "l", col = "red",lwd=2)
lines(x = tr_left_2_COPy$data_point, y = tr_lect_2_COPy$force,type = "l", col = "green",lwd=2)
legend("topright",legend= c("COPx","COPy"),col=c("red","green"),bg="white",lwd=2)
# axis(1,at=c(1:2),labels= c("COPx","COPy"))


#GRFx, GRFy and GRFz
plot(x = tr_left_2_GRFz$data_point, y = tr_lect_2_GRFz$force,type = "l", col = "blue",lwd=2,ylim =c(-200, 800))
lines(x = tr_left_2_GRFy$data_point, y = tr_lect_2_GRFy$force,type = "l", col = "green",lwd=2)
lines(x = tr_left_2_GRFx$data_point, y = tr_left_2_GRFx$force, type = "l", col = "red",lwd=2)
grid()
legend("topright",legend= c("GRFx", "GRFy","GRFz"),col=c("red","green","blue"),bg="white",lwd=2)
# axis(1,at=c(1:3),labels= c("GRFx", "GRFy","GRFz"))


#Mx, My, Mz
plot(x = tr_left_2_Mz$data_point, y = tr_left_2_Mz$force,type = "l", col = "blue",lwd=2, ylim = c(-60,25))
lines(x = tr_left_2_My$data_point, y = tr_left_2_My$force,type = "l", col = "green",lwd=2)
lines(x = tr_left_2_Mx$data_point, y = tr_left_2_Mx$force, type = "l", col = "red",lwd=2)
grid()
legend("topright",legend= c("Mx", "My","Mz"),col=c("red","green","blue"),bg="white",lwd=2)
# axis(1,at=c(1:3),labels= c("Mx", "My","Mz"))



# Main goal: time series dimension reduction


