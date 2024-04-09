#install.packages('FSelectorRcpp')
library(parallel)
pacotes <- c("tidyverse","PerformanceAnalytics", "Hmisc", "psych", "ggplot2",
             "gridExtra", "moments", "goeveg", "SnowballC", "randomForest",
             "caret", "stats", "lsa")



if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

suppressMessages(library(e1071))

# library(tidyverse)
# library(data.table)
# library(ggplot2)
# #library(cowplot)
# library(gridExtra)
# #library(plotly)
# library(moments)
# library(goeveg)
# library(PerformanceAnalytics)
# library(psych)
# library(randomForest)
# library(caret)
# library(stats)
# library(lsa)

#############
# constants #
#############

set.seed(42)

SWIPE_FILE <- 'swipeData.csv'  
KEYBOARD_FILE <- 'keyboardData.csv'  
SENSORS_FILE <- 'sensorsData.csv'

PATH_PARENT <- '/home/jcscabral/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/'

# All data
FILES_A <- c('20240119', '20240122_20', '20240123', '20240124',
               '20240125', '20240126', '20240228', '20240310')
# Only data with correct keyboard sensors
FILES_B <- c('20240124', '20240125', '20240126', '20240228', '20240310')

KEYBOARD_LOGIN <- 5
KEYBOARD_AUTH <- 6
SWIPE_HOME <- 0
SWIPE_PIX_SEND <- 2 

TYPE_ACCELEROMETER <- 1
TYPE_MAGNETIC_FIELD <- 2
TYPE_GYROSCOPE <- 4

################
# load scripts #
################

source('fix_bug_20240119.R')
source('fix_bug_20240122_20.R')

#############
# functions #
#############

# Quadratic Mean
qm <- function(x){
  x.clean <- x[!is.na(x)]
  r <- sqrt(sum(x.clean ^ 2)/length(x.clean))
  return(r)
} 

# Range
rg <- function(x){
  r <-range(x, na.rm = T)
  return(r[2] - r[1])
}

#Percentil
pc <- function(x, p){
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[1])
}

#Interquartile
qt <- function(x, nq){
  p <- c(0.0, 0.25, 0.5, 0.75,1 )
  x.na <- x[!is.na(x)]
  q <- quantile(x.na, probs = p)
  names(q) <- NULL
  return(q[nq + 1])
}

# Median Absolute Deviation
mi <- function(x){
  # median(|Yi – median(Yi|)
  x.na <- x[!is.na(x)]
  r <- median(abs(x.na - median(x.na)))
  return(r)
}

# Standard Error of Mean
se <- function(x){
  x.na <- x[!is.na(x)]
  r <- sd(x.na)/sqrt(length(x.na))
  return(r)
} 

# max deviation
mxdv <- function(x){
  x.na <- x[!is.na(x)]
  r <- max(sqrt((x.na - mean(x.na))^2))
  return(r)
}

# percentile of deviation
pcdv <- function(x, p){
  x.na <- x[!is.na(x)]
  r <- sqrt((x.na - mean(x.na))^2)
  return(pc(r, p))
}

# first
fs <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[1])
}

# last
lt <- function(x){
  #x.na <- x[!is.na(x)]
  return(x[length(x)])
}

# magnitude vector
mg <- function(x, y){
  r <- norm(c(x,y), type="2")
  return(r)
}


mg3 <- function(x, y, z){
  r <- norm(c(x,y,z), type="2")
  return(r)
}

# euclidian distance
ec <- function(x1, x2, y1, y2){
  r <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  return(r)
}

dif <- function(x){
  x0 <- fs(x)    
  x1 <- lt(x)
  return(x1 - x0)
}

ang <- function(x1, x2, y1, y2){
  r <- atan2((y2 - y1), (x2 - x1))
  return(r)
}

avang <- function(x, y){
  y.diff <- diff(y)
  x.diff <- diff(x)
  tangs <- y.diff/x.diff
  r <-  mean(atan(tangs))
  return(r)
}


mh <- function(xyz){
  return(
    mahalanobis(
      xyz,
      colMeans(xyz),
      cov(xyz)
    )
  )
}

#sensor stats
#((16 * 4) + 1 ) * 3

# conceptually correct
sensorStats <- function(dfdata){
  
  sensor_stats <- dfdata %>%
    mutate(
      acc_x = ifelse(sensor_type == 1, x, NA) ,
      acc_y = ifelse(sensor_type == 1, y, NA) ,
      acc_z = ifelse(sensor_type == 1, z, NA) ,
      acc_w = ifelse(sensor_type == 1, w, NA) ,
      mag_x = ifelse(sensor_type == 2, x, NA) ,
      mag_y = ifelse(sensor_type == 2, y, NA) ,
      mag_z = ifelse(sensor_type == 2, z, NA) ,
      mag_w = ifelse(sensor_type == 2, w, NA) ,
      gyr_x = ifelse(sensor_type == 4, x, NA) ,
      gyr_y = ifelse(sensor_type == 4, y, NA) ,
      gyr_z = ifelse(sensor_type == 4, z, NA) ,
      gyr_w = ifelse(sensor_type == 4, w, NA)
    ) %>%
    summarise(
      # Acc
      # x
      accx_md = median(acc_x, na.rm = T),
      accx_fq = qt(acc_x, 1),
      accx_tq = qt(acc_x, 3),
      accx_ir = IQR(acc_x, na.rm = T),
      accx_am = mean(acc_x, na.rm = T),
      accx_vr = var(acc_x, na.rm = T),
      accx_sd = sd(acc_x, na.rm = T),
      accx_cv = cv(acc_x, na.rm = T),
      accx_se = se(acc_x),
      accx_mn = min(acc_x, na.rm = T),
      accx_mx = max(acc_x, na.rm = T),
      accx_qm = qm(acc_x),
      accx_rg = rg(acc_x),
      accx_sk = skewness(acc_x, na.rm = T),
      accx_ku = kurtosis(acc_x, na.rm = T),
      accx_sm = sum(acc_x, na.rm = T),
      # y
      accy_md = median(acc_y, na.rm = T),
      accy_fq = qt(acc_y, 1),
      accy_tq = qt(acc_y, 3),
      accy_ir = IQR(acc_y, na.rm = T),
      accy_am = mean(acc_y, na.rm = T),
      accy_vr = var(acc_y, na.rm = T),
      accy_sd = sd(acc_y, na.rm = T),
      accy_cv = cv(acc_y, na.rm = T),
      accy_se = se(acc_y),
      accy_mn = min(acc_y, na.rm = T),
      accy_mx = max(acc_y, na.rm = T),
      accy_qm = qm(acc_y),
      accy_rg = rg(acc_y),
      accy_sk = skewness(acc_y, na.rm = T),
      accy_ku = kurtosis(acc_y, na.rm = T),
      accy_sm = sum(acc_y, na.rm = T),
      # z
      accz_md = median(acc_z, na.rm = T),
      accz_fq = qt(acc_z, 1),
      accz_tq = qt(acc_z, 3),
      accz_ir = IQR(acc_z, na.rm = T),
      accz_am = mean(acc_z, na.rm = T),
      accz_vr = var(acc_z, na.rm = T),
      accz_sd = sd(acc_z, na.rm = T),
      accz_cv = cv(acc_z, na.rm = T),
      accz_se = se(acc_z),
      accz_mn = min(acc_z, na.rm = T),
      accz_mx = max(acc_z, na.rm = T),
      accz_qm = qm(acc_z),
      accz_rg = rg(acc_z),
      accz_sk = skewness(acc_z, na.rm = T),
      accz_ku = kurtosis(acc_z, na.rm = T),
      accz_sm = sum(acc_z, na.rm = T),
      # w
      accw_md = median(acc_w, na.rm = T),
      accw_fq = qt(acc_w, 1),
      accw_tq = qt(acc_w, 3),
      accw_ir = IQR(acc_w, na.rm = T),
      accw_am = mean(acc_w, na.rm = T),
      accw_vr = var(acc_w, na.rm = T),
      accw_sd = sd(acc_w, na.rm = T),
      accw_cv = cv(acc_w, na.rm = T),
      accw_se = se(acc_w),
      accw_mn = min(acc_w, na.rm = T),
      accw_mx = max(acc_w, na.rm = T),
      accw_qm = qm(acc_w),
      accw_rg = rg(acc_w),
      accw_sk = skewness(acc_w, na.rm = T),
      accw_ku = kurtosis(acc_w, na.rm = T),
      accw_sm = sum(acc_w, na.rm = T),
      # all vectors 
      accsdp_mg = mg3(acc_x,acc_y,acc_z),
      #Mag
      # x
      magx_md = median(mag_x, na.rm = T),
      magx_fq = qt(mag_x, 1),
      magx_tq = qt(mag_x, 3),
      magx_ir = IQR(mag_x, na.rm = T),
      magx_am = mean(mag_x, na.rm = T),
      magx_vr = var(mag_x, na.rm = T),
      magx_sd = sd(mag_x, na.rm = T),
      magx_cv = cv(mag_x, na.rm = T),
      magx_se = se(mag_x),
      magx_mn = min(mag_x, na.rm = T),
      magx_mx = max(mag_x, na.rm = T),
      magx_qm = qm(mag_x),
      magx_rg = rg(mag_x),
      magx_sk = skewness(mag_x, na.rm = T),
      magx_ku = kurtosis(mag_x, na.rm = T),
      magx_sm = sum(mag_x, na.rm = T),
      # y
      magy_md = median(mag_y, na.rm = T),
      magy_fq = qt(mag_y, 1),
      magy_tq = qt(mag_y, 3),
      magy_ir = IQR(mag_y, na.rm = T),
      magy_am = mean(mag_y, na.rm = T),
      magy_vr = var(mag_y, na.rm = T),
      magy_sd = sd(mag_y, na.rm = T),
      magy_cv = cv(mag_y, na.rm = T),
      magy_se = se(mag_y),
      magy_mn = min(mag_y, na.rm = T),
      magy_mx = max(mag_y, na.rm = T),
      magy_qm = qm(mag_y),
      magy_rg = rg(mag_y),
      magy_sk = skewness(mag_y, na.rm = T),
      magy_ku = kurtosis(mag_y, na.rm = T),
      magy_sm = sum(mag_y, na.rm = T),
      # z
      magz_md = median(mag_z, na.rm = T),
      magz_fq = qt(mag_z, 1),
      magz_tq = qt(mag_z, 3),
      magz_ir = IQR(mag_z, na.rm = T),
      magz_am = mean(mag_z, na.rm = T),
      magz_vr = var(mag_z, na.rm = T),
      magz_sd = sd(mag_z, na.rm = T),
      magz_cv = cv(mag_z, na.rm = T),
      magz_se = se(mag_z),
      magz_mn = min(mag_z, na.rm = T),
      magz_mx = max(mag_z, na.rm = T),
      magz_qm = qm(mag_z),
      magz_rg = rg(mag_z),
      magz_sk = skewness(mag_z, na.rm = T),
      magz_ku = kurtosis(mag_z, na.rm = T),
      magz_sm = sum(mag_z, na.rm = T),
      # w
      magw_md = median(mag_w, na.rm = T),
      magw_fq = qt(mag_w, 1),
      magw_tq = qt(mag_w, 3),
      magw_ir = IQR(mag_w, na.rm = T),
      magw_am = mean(mag_w, na.rm = T),
      magw_vr = var(mag_w, na.rm = T),
      magw_sd = sd(mag_w, na.rm = T),
      magw_cv = cv(mag_w, na.rm = T),
      magw_se = se(mag_w),
      magw_mn = min(mag_w, na.rm = T),
      magw_mx = max(mag_w, na.rm = T),
      magw_qm = qm(mag_w),
      magw_rg = rg(mag_w),
      magw_sk = skewness(mag_w, na.rm = T),
      magw_ku = kurtosis(mag_w, na.rm = T),
      magw_sm = sum(mag_w, na.rm = T),
      # all vectors 
      magsdp_mg = mg3(mag_x,mag_y,mag_z),
      # gyr 
      # x
      gyrx_md = median(gyr_x, na.rm = T),
      gyrx_fq = qt(gyr_x, 1),
      gyrx_tq = qt(gyr_x, 3),
      gyrx_ir = IQR(gyr_x, na.rm = T),
      gyrx_am = mean(gyr_x, na.rm = T),
      gyrx_vr = var(gyr_x, na.rm = T),
      gyrx_sd = sd(gyr_x, na.rm = T),
      gyrx_cv = cv(gyr_x, na.rm = T),
      gyrx_se = se(gyr_x),
      gyrx_mn = min(gyr_x, na.rm = T),
      gyrx_mx = max(gyr_x, na.rm = T),
      gyrx_qm = qm(gyr_x),
      gyrx_rg = rg(gyr_x),
      gyrx_sk = skewness(gyr_x, na.rm = T),
      gyrx_ku = kurtosis(gyr_x, na.rm = T),
      gyrx_sm = sum(gyr_x, na.rm = T),
      # y
      gyry_md = median(gyr_y, na.rm = T),
      gyry_fq = qt(gyr_y, 1),
      gyry_tq = qt(gyr_y, 3),
      gyry_ir = IQR(gyr_y, na.rm = T),
      gyry_am = mean(gyr_y, na.rm = T),
      gyry_vr = var(gyr_y, na.rm = T),
      gyry_sd = sd(gyr_y, na.rm = T),
      gyry_cv = cv(gyr_y, na.rm = T),
      gyry_se = se(gyr_y),
      gyry_mn = min(gyr_y, na.rm = T),
      gyry_mx = max(gyr_y, na.rm = T),
      gyry_qm = qm(gyr_y),
      gyry_rg = rg(gyr_y),
      gyry_sk = skewness(gyr_y, na.rm = T),
      gyry_ku = kurtosis(gyr_y, na.rm = T),
      gyry_sm = sum(gyr_y, na.rm = T),
      # z
      gyrz_md = median(gyr_z, na.rm = T),
      gyrz_fq = qt(gyr_z, 1),
      gyrz_tq = qt(gyr_z, 3),
      gyrz_ir = IQR(gyr_z, na.rm = T),
      gyrz_am = mean(gyr_z, na.rm = T),
      gyrz_vr = var(gyr_z, na.rm = T),
      gyrz_sd = sd(gyr_z, na.rm = T),
      gyrz_cv = cv(gyr_z, na.rm = T),
      gyrz_se = se(gyr_z),
      gyrz_mn = min(gyr_z, na.rm = T),
      gyrz_mx = max(gyr_z, na.rm = T),
      gyrz_qm = qm(gyr_z),
      gyrz_rg = rg(gyr_z),
      gyrz_sk = skewness(gyr_z, na.rm = T),
      gyrz_ku = kurtosis(gyr_z, na.rm = T),
      gyrz_sm = sum(gyr_z, na.rm = T),
      # w
      gyrw_md = median(gyr_w, na.rm = T),
      gyrw_fq = qt(gyr_w, 1),
      gyrw_tq = qt(gyr_w, 3),
      gyrw_ir = IQR(gyr_w, na.rm = T),
      gyrw_am = mean(gyr_w, na.rm = T),
      gyrw_vr = var(gyr_w, na.rm = T),
      gyrw_sd = sd(gyr_w, na.rm = T),
      gyrw_cv = cv(gyr_w, na.rm = T),
      gyrw_se = se(gyr_w),
      gyrw_mn = min(gyr_w, na.rm = T),
      gyrw_mx = max(gyr_w, na.rm = T),
      gyrw_qm = qm(gyr_w),
      gyrw_rg = rg(gyr_w),
      gyrw_sk = skewness(gyr_w, na.rm = T),
      gyrw_ku = kurtosis(gyr_w, na.rm = T),
      gyrw_sm = sum(gyr_w, na.rm = T),
      # all vectors 
      gyrsdp_mg = mg3(gyr_x,gyr_y,gyr_z)
    )
  return(sensor_stats)
}  

# in theory, it doesn't make sense 
# sensorStats <- function(dfdata){
# 
#   sensor_stats <- dfdata %>%
#     summarise(
#       # Spacial x
#       sx_md = median(x, na.rm = T),
#       sx_fq = qt(x, 1),
#       sx_tq = qt(x, 3),
#       sx_ir = IQR(x, na.rm = T),
#       sx_am = mean(x, na.rm = T),
#       sx_vr = var(x, na.rm = T),
#       sx_sd = sd(x, na.rm = T),
#       sx_cv = cv(x, na.rm = T),
#       sx_se = se(x),
#       sx_mn = min(x, na.rm = T),
#       sx_mx = max(x, na.rm = T),
#       sx_qm = qm(x),
#       sx_rg = rg(x),
#       sx_sk = skewness(x),
#       sx_ku = kurtosis(x),
#       sx_sm = sum(x),
# 
#       # Spacial y
#       sy_md = median(y, na.rm = T),
#       sy_fq = qt(y, 1),
#       sy_tq = qt(y, 3),
#       sy_ir = IQR(y, na.rm = T),
#       sy_am = mean(y, na.rm = T),
#       sy_vr = var(y),
#       sy_sd = sd(y, na.rm = T),
#       sy_cv = cv(y, na.rm = T),
#       sy_se = se(y),
#       sy_mn = min(y, na.rm = T) ,
#       sy_mx = max(y, na.rm = T) ,
#       sy_qm = qm(y),
#       sy_rg = rg(y) ,
#       sy_sk = skewness(y) ,
#       sy_ku = kurtosis(y),
#       sy_sm = sum(y),
# 
#       # Spacial z
#       sz_md = median(z, na.rm = T),
#       sz_fq = qt(z, 1),
#       sz_tq = qt(z, 3),
#       sz_ir = IQR(z, na.rm = T),
#       sz_am = mean(z, na.rm = T),
#       sz_vr = var(z),
#       sz_sd = sd(z, na.rm = T),
#       sz_cv = cv(z, na.rm = T),
#       sz_se = se(z),
#       sz_mn = min(z, na.rm = T) ,
#       sz_mx = max(z, na.rm = T) ,
#       sz_qm = qm(z),
#       sz_rg = rg(z) ,
#       sz_sk = skewness(z) ,
#       sz_ku = kurtosis(z),
#       sz_sm = sum(z),
#       # all vectors
#       sdp_mg = mg3(x,y,z)
#     )
#   return(sensor_stats)
# }
#   
# intuition:
# the more variables from sensors, 
# more good variables from keyboard are disguised