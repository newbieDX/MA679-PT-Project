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

# First, we took look at the trial length distribution for different knees 
# to see if there exits some significant difference between each knee.
# The two vertical lines are median trial length for different knee:

trial_length <- ggplot(ID_info, aes(x = tr_length, fill = KNEE))+
  geom_density(alpha = 0.5,adjust = 0.3)+
  geom_vline(data = median_trlength, aes(xintercept = median, color = KNEE), size=1)+
  labs(x= "Trial length",
       subtitle="Trial length distribution for different knee")+
  theme(legend.position="bottom")

trial_length

# We also plot bar plot to check the distribution of trial number for different knees

trial_var <- ggplot(ID_info, aes(x = TRIAL, fill = KNEE))+
  geom_bar(stat = "count", position = "dodge")+
  labs(x= "Trial number",
       subtitle="Trial number distribution for different knee")

trial_var


# As we can see, there is no big significant different between two knees, we plan to check the success trial number proportion
# The total number of trial we select is 10:

succes_trial <- as.data.frame(table(ID_info$ID, ID_info$TRIAL)) %>% 
  pivot_wider(names_from = Var2, values_from = Freq) %>% 
  mutate(nr_success = rowSums(across(where(is.numeric)))) %>%
  mutate(success_prop = round(nr_success/10, 2))

# We can see that majority participants finish more than 5 trial combine with right and left knee. 
# This could be a potential feature to determine if the participant has some injures on his/her knee

ggplot(succes_trial, aes(x = success_prop))+
  geom_bar(stat = "count", position = "dodge")+
  ggtitle("Success proportion of 10 trials for each participant")


# For the features in the discrete df, we plan to look into TO-angle and vGRF first:
# take a look at TO-angle, we summarize an average TO-angle per knee for each participant group by ID and Trial.
mean_toangle <- ID_discrete %>% group_by(ID,TRIAL) %>% summarise(mean = mean(TO_angle))

# From the distribution of TO-angle, we can see it is almost normal distribution.
hist(mean_toangle$mean)



# take a look at vGRF:
# Beacuse from the info session, kerry mentioned that if the curve is flat or has extremely sharp turn, the participant might have some injure on his/her knee
# We plot the proportion of vGRF_valley to the mean of two peaks:
peak_valley <- ID_discrete %>% select(c(1:4, 16:23))
peak_valley_mean <- peak_valley %>%  group_by(ID,TRIAL) %>% summarise_at(vars(vGRF_peak1:mlGRF_peak3), mean) %>% 
  mutate(vGRF_valley_perc = vGRF_valley/((vGRF_peak1 + vGRF_peak2)/2))

# From the distribution, we can see there exists some flat(proportion > 0.9?) and some has extremely sharp turn(proportion < 0.6?). 
# But need more info like normal range to interpret.
hist(peak_valley_mean$vGRF_valley_perc,
     main="vGRF_valley / average vGRF_peaks",
     xlab="Proportion")

# rbind ID_info and discrete

ID_discrete <- cbind(ID_info, discrete)

# The next step we look into the standard deviation for each feature in discrete

sd_discrete <- as.data.frame(as.table(apply(discrete,2,sd)))

colnames(sd_discrete) <- c("Features", "SD")

sd_discrete <- ggplot(sd_discrete, aes(x = SD, y = Features))+
  geom_point(color = "red")+
  geom_text(aes(label = round(SD,2), hjust = -0.2))+
  xlim(0,5500)+
  labs(x= "Standard deviation",
       y = "Feature name",
       subtitle="Standard deviation for each feature")
  
sd_discrete

summary(discrete)

# According to the df we have stores the value of standard deviation for each value for discrete df, we can find out that
# vGRF_iLR_max  vGRF_iULR_max vGRF_avgLR    vGRF_avgULR
# These four features have relatively high sd

# Then we select 7 features to generate a PCA
#PCA on discrete:
discrete.pca <- prcomp(ID_discrete[,c(12:18)], center = TRUE,scale. = TRUE)
summary(discrete.pca)


library(devtools)
# install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(discrete.pca, 
         alpha =0.5, 
         choices = c(1,2),
         varname.adjust = 4, 
         circle = TRUE,
         obs.scale = 1, 
         var.scale = 1,
         ellipse = TRUE,
         var.axes = T)+
  xlim(-15,25)+
  ylim(-10,25)


# Clean for time series data COPx:
# Use ID = 1 as an example:

ID_COPx <- cbind(ID_info, COPx)
ID_COPy <- cbind(ID_info, COPy)

t <- strptime("00:00:00","%H:%M:%S")+ 1*1:2990
data <- as.data.frame(t) %>% 
  mutate(data_point = as.character(c(1:2990))) 


data_wrangle_COPx <- function(df, id){
  df_1 <- subset(df, df$ID == id)
  df_1 <- df_1 %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data, by = c("data_point"))
  return(df_mock)
}

data_wrangle_COPy <- function(df, id){
  df_1 <- subset(df, df$ID == id)
  df_1 <- df_1 %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPy")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data, by = c("data_point"))
  return(df_mock)
}

# data wrangle COPx for participant id:1,2,3:
# COPx_ID_1_ts <- data_wrangle_COPx(ID_COPx, 1)
# COPx_ID_2_ts <- data_wrangle_COPx(ID_COPx, 2)
# COPx_ID_3_ts <- data_wrangle_COPx(ID_COPx, 3)

# data wrangle COPx_stance for participant id:1,2,3
# COPx_stance_ID_1_ts <- data_wrangle_COPx(ID_COPx_stance, 1)
# COPx_stance_ID_2_ts <- data_wrangle_COPx(ID_COPx_stance, 2)
# COPx_stance_ID_3_ts <- data_wrangle_COPx(ID_COPx_stance, 3)

# data wrangle COPy for participant id:1,2,3:
# COPy_ID_1_ts <- data_wrangle_COPy(ID_COPy, 1)
# COPy_ID_2_ts <- data_wrangle_COPy(ID_COPy, 2)
# COPy_ID_3_ts <- data_wrangle_COPy(ID_COPy, 3)

# data wrangle COPxy_stance for participant id:1,2,3
# COPy_stance_ID_1_ts <- data_wrangle_COPy(ID_COPy_stance, 1)
# COPy_stance_ID_2_ts <- data_wrangle_COPy(ID_COPy_stance, 2)
# COPy_stance_ID_3_ts <- data_wrangle_COPy(ID_COPy_stance, 3)

# ggplot

time_series_mock_plot_COPx <- function(df){
  ggplot(df, aes(x = t, y = COPx, color = TRIAL))+
    geom_line()+
    facet_grid(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}

time_series_mock_plot_COPy <- function(df){
  ggplot(df, aes(x = t, y = COPy, color = TRIAL))+
    geom_line()+
    facet_grid(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}


# ts plot COPx for participant id:1,2,3 
# time_series_mock_plot_COPx(COPx_ID_1_ts)+ggtitle("x dimension Center of pressure for participant ID 1")
# time_series_mock_plot_COPx(COPx_ID_2_ts)+ggtitle("x dimension Center of pressure for participant ID 2")
# time_series_mock_plot_COPx(COPx_ID_3_ts)+ggtitle("x dimension Center of pressure for participant ID 3")

# ts plot COPx_stance for participant id:1,2,3
# time_series_mock_plot_COPx(COPx_stance_ID_1_ts)+ggtitle("x dimension Center of pressure for participant ID 1")
# time_series_mock_plot_COPx(COPx_stance_ID_2_ts)+ggtitle("x dimension Center of pressure for participant ID 2")
# time_series_mock_plot_COPx(COPx_stance_ID_3_ts)+ggtitle("x dimension Center of pressure for participant ID 3")

# ts plot COPy for participant id:1,2,3 
# time_series_mock_plot_COPy(COPy_ID_1_ts)+ggtitle("y dimension Center of pressure for participant ID 1")
# time_series_mock_plot_COPy(COPy_ID_2_ts)+ggtitle("y dimension Center of pressure for participant ID 2")
# time_series_mock_plot_COPy(COPy_ID_3_ts)+ggtitle("y dimension Center of pressure for participant ID 3")

# ts plot COPx_stance for participant id:1,2,3
# time_series_mock_plot_COPy(COPy_stance_ID_1_ts)+ggtitle("x dimension Center of pressure for participant ID 1")
# time_series_mock_plot_COPy(COPy_stance_ID_2_ts)+ggtitle("x dimension Center of pressure for participant ID 2")
# time_series_mock_plot_COPy(COPy_stance_ID_3_ts)+ggtitle("x dimension Center of pressure for participant ID 3")


# plot:


# df_plot <- COPx_ID_2_ts %>% mutate(new = paste(ID, "-",KNEE, "-",TRIAL))
# 
# tr_left_2 <- subset(df_plot, df_plot$new == "2 - LEFT - 2")
# 
# plot(x = tr_left_2$data_point, y = tr_left_2$COPx, type = "l")



# time-normalize data COPx_stance:

t_2 <- strptime("00:00:00","%H:%M:%S")+ 1*1:100
data_2 <- as.data.frame(t_2) %>% 
  mutate(data_point = as.character(c(1:100))) 

ID_COPx_stance <- cbind(ID_info, COPx_stance)
ID_COPy_stance <- cbind(ID_info, COPy_stance)


data_wrangle_COPx_total <- function(df){
  df_1 <- df %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPx")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data_2, by = c("data_point")) %>%
    mutate(new = paste(ID, "-",KNEE, "-",TRIAL))
  return(df_mock)
}

data_wrangle_COPy_total <- function(df){
  df_1 <- df %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "data_point", values_to = "COPy")
  df_1$data_point <- gsub('^.', '', df_1$data_point)
  df_1$TRIAL <- as.character(df_1$TRIAL)
  df_mock <- left_join(df_1, data_2, by = c("data_point")) %>%
    mutate(new = paste(ID, "-",KNEE, "-",TRIAL))
  return(df_mock)
}


COPx_stance_ts <- data_wrangle_COPx_total(ID_COPx_stance)
COPy_stance_ts <- data_wrangle_COPy_total(ID_COPy_stance)

time_series_COPx_plot_total <- function(df){
  ggplot(df, aes(x = t_2, y = COPx, color = new))+
    geom_line()+
    facet_wrap(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}

time_series_COPy_plot_total <- function(df){
  ggplot(df, aes(x = t_2, y = COPy, color = new))+
    geom_line()+
    facet_wrap(~KNEE)+
    theme(legend.position = "none",
          axis.text.x=element_blank())
}


time_series_COPx_plot_total(COPx_stance_ts)
time_series_COPy_plot_total(COPy_stance_ts)

