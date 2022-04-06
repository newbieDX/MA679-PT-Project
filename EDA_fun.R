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

# Time-normalized data
AP_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/AP_GRF_stance_N.csv", header = F)
ML_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/ML_GRF_stance_N.csv", header = F)
V_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/V_GRF_stance_N.csv", header = F)
COPx_stance <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/COPx_stance.csv", header = F)
COPy_stance <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/COPy_stance.csv", header = F)

# EDA:
# ID_info
median_trlength <- ID_info %>% group_by(KNEE) %>% summarise(median = median(tr_length))
mean_trlength <- ID_info %>% group_by(KNEE) %>% summarise(mean = mean(tr_length))

ggplot(ID_info, aes(x = tr_length, fill = KNEE))+
  geom_density(alpha = 0.5,adjust = 0.3)+
  geom_vline(data = median_trlength, aes(xintercept = median, color = KNEE), size=1)+
  labs(x= "Trial length",
       subtitle="Trial length distribution for different knee")+
  theme(legend.position="bottom")

# rbind ID_info and discrete

ID_discrete <- cbind(ID_info, discrete)

sd_discrete <- as.data.frame(as.table(apply(discrete,2,sd)))

colnames(sd_discrete) <- c("Features", "SD")

ggplot(sd_discrete, aes(x = SD, y = Features))+
  geom_point()

# According to the df we have stores the value of standard deviation for each value for discrete df, we can find out that
# vGRF_iLR_max  vGRF_iULR_max vGRF_avgLR    vGRF_avgULR
# These four features have relatively high sd
# So we decide to see the distribution for there four features according to each knee

median_vGRF_iLR_max <- ID_discrete %>% group_by(KNEE) %>% summarise(median = median(vGRF_iLR_max))
mean_vGRF_iLR_max <- ID_discrete %>% group_by(KNEE) %>% summarise(mean = mean(vGRF_iLR_max))

ggplot(ID_discrete, aes(y = ID, x = vGRF_iLR_max))+
  geom_point()
# geom_vline(data = median_vGRF_iLR_max, aes(xintercept = median, color = KNEE), size=1)+
# labs(x= "vGRF_iLR_max",
#      subtitle="vGRF_iLR_max distribution for different knee")+
# theme(legend.position="bottom")

ggplot(ID_discrete, aes(x = vGRF_iULR_max, y = ID))+
  geom_point()
# geom_vline(data = median_vGRF_iLR_max, aes(xintercept = median, color = KNEE), size=1)+
# labs(x= "vGRF_iLR_max",
#      subtitle="vGRF_iLR_max distribution for different knee")+
# theme(legend.position="bottom")

# From the sd df, we can know that 


# PCA on discrete:
# discrete.pca <- prcomp(ID_discrete[,c(3:33)], center = TRUE,scale. = TRUE)
# summary(discrete.pca)
# 
# library(devtools)
# #install_github("vqv/ggbiplot")
# 
# library(ggbiplot)
# 
# ggbiplot(discrete.pca)


# Clean for time series data COPx:
# Use ID = 1 as an example:

ID_COPx <- cbind(ID_info, COPx)
# ID_COPx$ID <- as.character(ID_COPx$ID)
COPx_ID_1 <- head(ID_COPx,8)

COPx_ID_2 <- subset(ID_COPx, ID_COPx$ID == 2)

# ID_COPx_ts <- ID_COPx %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
# ID_COPx_ts$data_point <- gsub('^.', '', ID_COPx_ts$data_point)
# ID_COPx_ts$TRIAL <- as.character(ID_COPx_ts$TRIAL)
# ID_COPx_ts_mock <- left_join(ID_COPx_ts, data, by = c("data_point"))
# 
# ggplot(ID_COPx_ts_mock, aes(x = t, y = COPx, color = TRIAL))+
#   geom_line()+
#   facet_grid(~KNEE)+
#   theme(legend.position = "none")

COPx_ID_1_ts <- COPx_ID_1 %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
COPx_ID_1_T1_left_ts <- COPx_ID_1_ts %>% filter(COPx_ID_1_ts$TRIAL == 2 & COPx_ID_1_ts$KNEE == "LEFT") 
COPx_ID_1_T1_left_ts$data_point <- gsub('^.', '', COPx_ID_1_T1_left_ts$data_point)

t <- strptime("00:00:00","%H:%M:%S")+ 1*1:2990
data <- as.data.frame(t) %>% 
  mutate(data_point = as.character(c(1:2990))) 

# data$data_point <- c(1:2990) %>% as.character()

COPx_ID_1_ts$data_point <- gsub('^.', '', COPx_ID_1_ts$data_point)
COPx_ID_1_ts_mock <- left_join(COPx_ID_1_ts, data, by = c("data_point"))
COPx_ID_1_ts_mock$TRIAL <- as.character(COPx_ID_1_ts_mock$TRIAL)

ggplot(COPx_ID_1_ts_mock, aes(x = t, y = COPx, color = TRIAL))+
  geom_line()+
  facet_grid(~KNEE)+
  theme(legend.position = "none",
        axis.text.x=element_blank())


data_wrangle_COPx <- function(df, id){
  df_1 <- subset(df, df$ID == id)
  df_1 <- df_1 %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data, by = c("data_point"))
  return(df_mock)
}

COPx_ID_3_ts <- data_wrangle_COPx(ID_COPx, 3)


time_series_mock_plot <- function(df){
  ggplot(df, aes(x = t, y = COPx, color = TRIAL))+
    geom_line()+
    facet_grid(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}


time_series_mock_plot(COPx_ID_3_ts)

# time-normalize data COPx_stance:

t_2 <- strptime("00:00:00","%H:%M:%S")+ 1*1:100
data_2 <- as.data.frame(t_2) %>% 
  mutate(data_point = as.character(c(1:100))) 

ID_COPx_stance <- cbind(ID_info, COPx_stance)

COPx_stance_ID_1_ts <- data_wrangle_COPx(ID_COPx_stance, 1)
time_series_mock_plot(COPx_stance_ID_1_ts)

data_wrangle_COPx_total <- function(df){
  df_1 <- df %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data_2, by = c("data_point"))
  return(df_mock)
}

time_series_mock_plot_2 <- function(df){
  ggplot(df, aes(x = t_2, y = COPx, color = TRIAL))+
    geom_line()+
    facet_grid(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}

COPx_stance_ts <- data_wrangle_COPx_total(ID_COPx_stance)

time_series_mock_plot_2(COPx_stance_ts)