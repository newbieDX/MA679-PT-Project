#load data:
ID_info <- read_excel("/Users/xuchen/Desktop/MA679/PTProject_Data/IDinfo.xls")
# Time-normalized data
AP_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/AP_GRF_stance_N.csv", header = FALSE)
ML_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/ML_GRF_stance_N.csv", header = FALSE)
V_GRF_stance_N <- read.csv("/Users/xuchen/Desktop/MA679/PTProject_Data/V_GRF_stance_N.csv", header = FALSE)

data_pre_func <- function(df1, df2){
  df <- cbind(df1, df2)
  df3 <- df %>% pivot_longer(-c(ID, KNEE, TRIAL, tr_length), names_to = "time", values_to = "force")%>%
    mutate(new = paste(ID, "-",KNEE, "-",TRIAL)) 
  
  t <- rep(seq(0,100,length.out = 100),nrow(df))
  
  df_pre <- cbind(df3, t)
  return(df_pre)
}

ap_fpca <- data_pre_func(ID_info, AP_GRF_stance_N)
ml_fpca <- data_pre_func(ID_info, ML_GRF_stance_N)
v_fpca <- data_pre_func(ID_info, V_GRF_stance_N)


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



mat_ap <- as.matrix(AP_GRF_stance_N)
mat_ml <- as.matrix(ML_GRF_stance_N)
mat_v <- as.matrix(V_GRF_stance_N)

color <-  rgb(runif(15696,0,1),runif(15696,0,1), runif(15696,0,1))

ml_ts <- function(df){
  plot(df[1,], col = color[1], type = 'l', lwd = 1, ylim = c(-150,200), xlab = "Time point", ylab = "ML_GRF_stance_N")
  for (i in 2:nrow(df)) {
    lines(df[i,], col = color[i], type = 'l', lwd = 1)
  }
}

ap_ts <- function(df){
  plot(df[1,], col = color[1], type = 'l', lwd = 1, ylim = c(-400,400), xlab = "Time point", ylab = "AP_GRF_stance_N")
  for (i in 2:nrow(df)) {
    lines(df[i,], col = color[i], type = 'l', lwd = 1)
  }
}

v_ts <- function(df){
  plot(df[1,], col = color[1], type = 'l', lwd = 1, ylim = c(0,2000), xlab = "Time point", ylab = "V_GRF_stance_N")
  for (i in 2:nrow(df)) {
    lines(df[i,], col = color[i], type = 'l', lwd = 1)
  }
}


