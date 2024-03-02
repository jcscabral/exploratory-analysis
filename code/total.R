#install.packages("PerformanceAnalytics")
#install.packages("Hmisc")
install.packages("psych")

library(tidyverse)
library(data.table)
library(ggplot2)
library(moments)
library(goeveg)
library(plotly)
library(PerformanceAnalytics)
library(psych)

swipe_file <- 'swipeData.csv'  
keyboard_file <- 'keyboardData.csv'  
sensors_file <- 'sensorsData.csv'

path_parent <- '/home/jcscabral/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/'

files <- c('20240123', '20240124', '20240125', '20240126', '20240228')


############
# KEYBOARD #
############


keyboard.total = NULL

for(file in files){
  
  path_day <- paste(path_parent, file, '/', sep = '')
  
  full_path <- paste(path_day, keyboard_file, sep = '')
  keyboard <- fread(full_path)
  cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
              'character', 'pressure', 'x', 'y', 'uptime')  
  names(keyboard) <- cnames
  
  ids_completed <- unique(keyboard[keyboard$action_number == 7]$user_id)
  keyboard.completed <- keyboard %>% filter(user_id %in% ids_completed)
  
  # it started with user_id == 51
  if (file == '20240228'){
    keyboard[keyboard$user_id>50]
  }
  if(is.null(keyboard.total)){
    keyboard.total <- keyboard.completed
  }
  else{
    keyboard.total <-rbind(keyboard.total, keyboard.completed)
  }
}

keyboard <- NULL
keyboard.completed <- NULL

ids_completed <- unique(keyboard.total$user_id)

keyboard <- keyboard.total %>% filter(user_id %in% ids_completed)
keyboard.total =  NULL

keyboard <- rbind(keyboard, keyboard_data_20240119())
keyboard <- rbind(keyboard, keyboard_data_20240122_20())

ids_completed <- sort(unique(keyboard$user_id))


# new variables

keyboard$time_diff <- 0
keyboard$press_time_diff <- 0
keyboard$pressure_var <- 0.0 # new
keyboard$space_x <- 0.0
keyboard$space_y <- 0.0
keyboard$velocity_x <- 0.0
keyboard$velocity_y <- 0.0

for (id in ids_completed){
  
  id_indices <- which(keyboard$user_id == id)
  
  current_action <- -1
  
  for (indice in id_indices){
    
    former_indice <- indice - 1
    
    # app_action
    if(current_action != keyboard[indice]$app_action){
      keyboard[indice]$time_diff <- 0
      current_action <- keyboard[indice]$app_action
    } else{
      keyboard[indice]$time_diff <- diff(
        keyboard[former_indice:indice]$uptime)
    }
    
    # event_type_pointer
    current_event_type <- keyboard[indice]$pointer_event_type
    if (current_event_type == 1){ 
      keyboard[indice]$press_time_diff <- 0
      keyboard[indice]$pressure_var <- 0.0
      keyboard[indice]$velocity_x <- 0.0
      keyboard[indice]$velocity_y <- 0.0
    } else {
      
      keyboard[indice]$press_time_diff <- diff(
        keyboard[former_indice:indice]$uptime)
      
      keyboard[indice]$pressure_var <- diff(
        keyboard[former_indice:indice]$pressure) /
        keyboard[indice]$press_time_diff
      
      keyboard[indice]$space_x <- diff(keyboard[former_indice:indice]$x)
      
      keyboard[indice]$space_y <- diff(keyboard[former_indice:indice]$y)
      
      keyboard[indice]$velocity_x <- diff(
        keyboard[former_indice:indice]$space_x)/
          keyboard[indice]$press_time_diff
      
      keyboard[indice]$velocity_y <- diff(
        keyboard[former_indice:indice]$space_y)/
          keyboard[indice]$press_time_diff
    }
  }
}


# --------------------------------------------------------------
# teh2019 
# SOF second-order features

# Minimum (mn), Maximum (mx), Arithmetic
# Mean (am), Quadratic Mean (qm), Harmonic Mean (hm),
# Geometric Mean (gm), Median (md), Range (rg), Variance
# (vr), Standard Deviation (sd), Skewness (sk), Kurtosis (ku),
# First Quartile (fq), Third Quartile (tq), Interquartile Range
# (ir), Mean Absolute Deviation (ma), Median Absolute Deviation (mi),
# Coefficient of Variation (cv), and Standard Error of Mean (se).
# --------------------------------------------------------------

# more variables ... TODO
# first, last
# ------------

# functions

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

#########
# login #
#########

keyboard_login <- keyboard %>%
  filter(app_action == 5 | app_action == 6)

base <- keyboard_login %>%
  group_by(user_id, character)


# All characters

# 15 * 5 * 8 features

# General ----
desc_login_character <- base %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA)
    ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )

# password: 54831790


# By character




# Character 5 ------
desc_login_character_5 <- base %>%
  filter(character == 5) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 4 ------
desc_login_character_4 <- base %>%
  filter(character == 4) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 8 ------
desc_login_character_8 <- base %>%
  filter(character == 8) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 3 ------
desc_login_character_3 <- base %>%
  filter(character == 3) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 1 ------
desc_login_character_1 <- base %>%
  filter(character == 1) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 7 ------
desc_login_character_7 <- base %>%
  filter(character == 1) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 9 ------
desc_login_character_9 <- base %>%
  filter(character == 9) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )


# Character 0 ------
desc_login_character_0 <- base %>%
  filter(character == 0) %>%
  mutate(
    fly_time = ifelse(pointer_event_type == 1, time_diff, NA) ,
    dwell_time = ifelse(pointer_event_type != 1, press_time_diff, NA) , 
  ) %>%
  summarise(
    # DT Dwelling Time
    dt_md = median(dwell_time, na.rm = T),
    dt_fq = qt(dwell_time, 1),
    dt_tq = qt(dwell_time, 3),
    dt_ir = IQR(dwell_time, na.rm = T),
    dt_ma = mean(dwell_time, na.rm = T),
    dt_vr = var(dwell_time, na.rm = T),
    dt_sd = sd(dwell_time, na.rm = T),
    dt_cv = cv(dwell_time, na.rm = T),
    dt_se = se(dwell_time),
    dt_mn = min(dwell_time, na.rm = T) ,
    dt_mx = max(dwell_time, na.rm = T) ,
    dt_qm = qm(dwell_time) ,
    dt_rg = rg(dwell_time) ,
    dt_sk = skewness(dwell_time) ,
    dt_ku = kurtosis(dwell_time) ,
    # FT Flight Time
    ft_md = median(fly_time, na.rm = T),
    ft_fq = qt(fly_time, 1),
    ft_tq = qt(fly_time, 3),
    ft_ir = IQR(fly_time, na.rm = T),
    ft_am = mean(fly_time, na.rm = T),
    ft_vr = var(fly_time, na.rm = T),
    ft_sd = sd(fly_time, na.rm = T),
    ft_cv = cv(fly_time, na.rm = T),
    ft_se = se(fly_time),
    ft_mn = min(fly_time , na.rm = T) ,
    ft_mx = max(fly_time, na.rm = T) ,
    dt_qm = qm(fly_time) ,
    ft_rg = rg(fly_time) ,
    ft_sk = skewness(fly_time) ,
    ft_ku = kurtosis(fly_time) ,
    # Pressure
    ps_md = median(pressure),
    ps_fq = qt(pressure, 1),
    ps_tq = qt(pressure, 3),
    ps_ir = IQR(pressure, na.rm = T),
    ps_am = mean(pressure, na.rm = T),
    ps_var = var(pressure, na.rm = T),
    ps_sd = sd(pressure, na.rm = T),
    ps_cv = cv(pressure, na.rm = T),
    ps_se = se(pressure),
    ps_mn = min(pressure),
    ps_mx = max(pressure),
    ps_qm = qm(pressure) ,
    ps_rg = rg(pressure) ,
    ps_sk = skewness(pressure) ,
    ps_ku = kurtosis(pressure) ,
    # Spacial x
    x_md = median(x, na.rm = T),
    x_fq = qt(x, 1),
    x_tq = qt(x, 3),
    x_ir = IQR(x, na.rm = T),
    x_am = mean(x, na.rm = T),
    x_vr = var(x, na.rm = T),
    x_sd = sd(x, na.rm = T),
    x_cv = cv(x, na.rm = T),
    x_se = se(x),
    x_mn = min(x, na.rm = T) ,
    x_mx = max(x, na.rm = T) ,
    x_qm = qm(x) ,
    x_rg = rg(x),
    x_sk =  skewness(x) ,
    x_ku = kurtosis(x) ,
    # Spacial y
    y_md = median(y, na.rm = T),
    y_fq = qt(y, 1),
    y_tq = qt(y, 3),
    y_ir = IQR(y, na.rm = T),
    y_am = mean(y, na.rm = T),
    y_vr = var(y),
    y_sd = sd(y, na.rm = T),
    y_cv = cv(y, na.rm = T),
    y_se = se(y),
    y_mn = min(y, na.rm = T) ,
    y_mx = max(y, na.rm = T) ,
    y_qm = qm(y),
    y_rg =  rg(y) ,
    y_sk =  skewness(y) ,    
    y_ku = kurtosis(y)
  )
















base <- keyboard_login %>%
  group_by(user_id, pointer_event_type, action_number)

description_login <- base %>%
  summarise(
    median_time = median(press_time_diff),
    mean_time = mean(time_diff),
    sd_time = sd(time_diff),
    median_pressure = median(pressure),
    mean_pressure = mean(pressure),
    sd_pressure = var(pressure),
  )

# plot #

userid <- 25
#plot(description_login[description_login$user_id == user_id,]$median_pressure)
#plot(keyboard[keyboard$user_id == userid,]$pressure)

usereventtype <- keyboard %>% 
  filter((user_id == userid) &
           pointer_event_type == 1)

plot(usereventtype$pressure)


# coefficient of variation (NRMSD) #

base <- keyboard_login %>%
  group_by(user_id, action_number)

description_login <- base %>%
  summarise(
    sd_time = sd(press_time_diff, na.rm = T),
    mean_time = mean(press_time_diff),
    cv_time = (sd_time / mean_time) * 100,
    sd_pressure = sd(pressure, na.rm = T),
    mean_pressure = mean(pressure),
    cv_pressure = (sd_pressure/ mean_pressure) * 100
  )


# grouped #

# by character

base <- keyboard %>%
  group_by(user_id, character)

description_character <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    first_press_time = first(time_diff),
    obs = n()
  )


# all characters


base <- keyboard %>%
  group_by(user_id, app_action, action_number)

description_action <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    obs = n()
  )

base <- keyboard %>%
  group_by(user_id, pointer_event_type)

description_pointer <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    obs = n()
  )





# Renaming 
col_char5 <- function(x){
  return(paste(x, "_5", '', sep= ''))
}
col_char4 <- function(x){
  return(paste(x, "_4", '', sep= ''))
}
col_char8 <- function(x){
  return(paste(x, "_8", '', sep= ''))
}
col_char3 <- function(x){
  return(paste(x, "_3", '', sep= ''))
}
col_char1 <- function(x){
  return(paste(x, "_1", '', sep= ''))
}
col_char7 <- function(x){
  return(paste(x, "_7", '', sep= ''))
}
col_char9 <- function(x){
  return(paste(x, "_9", '', sep= ''))
}
col_char0 <- function(x){
  return(paste(x, "_0", '', sep= ''))
}

col.names <- names(desc_login_character_5)
col.names <- col.names[3:length(col.names)]

desc_login_character_5 <- desc_login_character_5 %>% 
  rename_with(.fn = col_char5, .cols = col.names)

desc_login_character_4 <- desc_login_character_4 %>% 
  rename_with(.fn = col_char4, .cols = col.names)

desc_login_character_8 <- desc_login_character_8 %>% 
  rename_with(.fn = col_char8, .cols = col.names)

desc_login_character_3 <- desc_login_character_3 %>% 
  rename_with(.fn = col_char3, .cols = col.names)

desc_login_character_1 <- desc_login_character_1 %>% 
  rename_with(.fn = col_char1, .cols = col.names)

desc_login_character_7 <- desc_login_character_7 %>% 
  rename_with(.fn = col_char7, .cols = col.names)

desc_login_character_9 <- desc_login_character_9 %>% 
  rename_with(.fn = col_char9, .cols = col.names)

desc_login_character_0 <- desc_login_character_0 %>% 
  rename_with(.fn = col_char0, .cols = col.names)

# unique w/ all characters

desc_login_characters <- inner_join(
  desc_login_character_5, desc_login_character_4, by = "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_8, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_3, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_1, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_7, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_9, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL
desc_login_characters <- inner_join(
  desc_login_characters, desc_login_character_0, by= "user_id")
desc_login_characters$character.x <- NULL
desc_login_characters$character.y <- NULL


######################
# Feature Extraction #
######################

colSums(is.na(desc_login_characters))
cols.na <-names(
  which(colSums(is.na(desc_login_characters)) == 0)
  )
# named as a card
card_chars <- desc_login_characters[cols.na]


# Correlation

rho <- Hmisc::rcorr(
  as.matrix(card_chars[2:nrow(card_chars)]), type = "pearson"
)
corr_coef <- rho$r
corr_sig <- round(rho$P, 5)

chart.Correlation(card_chars[2:11])
chart.Correlation(card_chars[12:21])
chart.Correlation(card_chars[22:31])
chart.Correlation(card_chars[32:nrow(card_chars)])


# Information Gain Attribute Evaluator (IGAE) Weka

card_chars$id <- as.character(card_chars$user_id)
write_csv(card_chars[c(ncol(card_chars), c(2:590))], 'card_keyboard.csv')

# Ranked attributes by Weka InfoGainAttributeVal:

cols_ranked <- c("user_id", "y_sd_5", "y_vr_5", "ps_qm_8", "y_rg_5",
  "ps_md_0", "ps_am_0", "ps_am_3", "y_mn_9", "y_fq_9", "dt_ir_5",
  "y_cv_9", "dt_qm_1", "dt_qm_7")

card_chars_ranked <- card_chars[cols_ranked]

# CLASSIFIERS

# Buriro, 

# Naive Bayes
# NeuralNet (NN)
# RF (best results according paper)


# RF Random Forrest #

install.packages("randomForest")
install.packages("caret")

library(randomForest)
library(caret)

ind <- round(nrow(card_chars_ranked) * 0.90)

train <- as.data.frame(card_chars_ranked[1:ind,])
x_train <- train[,2:length(cols_ranked)]
y_train <- as.factor(train[,1])

test <- as.data.frame(
  card_chars_ranked[(ind + 1): nrow(card_chars_ranked),])
x_test <- test[,2:length(cols_ranked)]
y_test <- as.factor(test[,1])

rf <- randomForest(x_train, y_train, ntree = 10)

results <- predict(object = rf, x_test, type ="prob")
results

apply(results, 1, max, na.rm=TRUE) # max by row

nrow(train)











# PCA
# Before select variables, each data user is transformed 
# in order to preserve its specific features.  

fatorial <- principal(card_chars[2:11] ,
          nfactors = length(card_chars[2:11]),
          rotate = "none",
          scores = T)

eigenvalues <- round(fatorial$values, 5)
var_shared <-as.data.frame(fatorial$Vaccounted)  
fatorials_scores <- as.data.frame(fatorial$weights)
factors <- fatorial$scores


rho.factors <- Hmisc::rcorr(as.matrix(factors), type= "pearson")
k <- sum(eigenvalues > 1)
fatorial2 <- principal(card_chars[2:11] ,
            nfactors = k,
            rotate = "none",
            scores = T)






















###########
# SENSORS #
###########

files.last <- c('20240124', '20240125', '20240126')

sensors.total = NULL

for(file in files.last){
  
  path_day <- paste(path_parent, file, '/', sep = '')
  
  # sensors #
  full_path <- paste(path_day, sensors_file, sep = '')
  sensors <- fread(full_path)
  cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type', 'x', 'y', 'z', 'timestamp')  
  names(sensors) <- cnames
  
  ids_completed <- unique(sensors[sensors$action_number == 7]$user_id)
  sensors.completed <- sensors %>% filter(user_id %in% ids_completed)
  
  if(is.null(sensors.total)){
    sensors.total <- sensors.completed
  }
  else{
    sensors.total <-rbind(sensors.total, sensors.completed)
  }
}

sensors <- NULL
sensors.completed <- NULL

ids_completed <- unique(sensors.total$user_id)
length(ids_completed) #31

sensors <- sensors.total %>% filter(user_id %in% ids_completed)
sensors.total =  NULL

sensors$w <- sqrt((sensors$x^2) + (sensors$y^2) + (sensors$z^2))

sensors$timestamp <- sensors$timestamp/1000000
# 139332168 | 237742206570000

# 40
df <- sensors[(sensors$user_id == 40) & (sensors$sensor_type == 1),] %>%
  select(c(9, 8))
write.csv(df, '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors40')


# 41
write.csv(
  sensors[(sensors$user_id == 41) & (sensors$sensor_type == 1),] %>%
       select(c(9, 8)), 
  '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors41')

# 42
write.csv(
  sensors[(sensors$user_id == 42) & (sensors$sensor_type == 1),] %>%
    select(c(9, 8)), 
  '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors42')

# 50
write.csv(
  sensors[(sensors$user_id == 50) & (sensors$sensor_type == 1),] %>%
    select(c(9, 8)), 
  '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors50')


# 50
write.csv(
  sensors[(sensors$user_id == 50) & (sensors$sensor_type == 1),] %>%
    select(c(9, 8)), 
  '/home/jcscabral/Studies/UspEsalq/pesquisa/sensors50')



#########
# SWIPE #
#########

swipe.total = NULL

for(file in files){
  
  path_day <- paste(path_parent, file, '/', sep = '')
  
  # SWIPE #
  full_path <- paste(path_day, swipe_file, sep = '')
  swipe <- fread(full_path)
  cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 
              'pressure', 'x', 'y', 'uptime')  
  names(swipe) <- cnames
  
  ids_completed <- unique(swipe[swipe$action_number == 7]$user_id)
  swipe.completed <- swipe %>% filter(user_id %in% ids_completed)
  
  if(is.null(swipe.total)){
    swipe.total <- swipe.completed
  }
  else{
    swipe.total <-rbind(swipe.total, swipe.completed)
  }
}

swipe <- NULL
swipe.completed <- NULL

ids_completed <- unique(swipe.total$user_id)
length(ids_completed) #31

swipe <- swipe.total %>% filter(user_id %in% ids_completed)
swipe.total =  NULL

# new variables

swipe$swipe_number <- 0
swipe$time_diff <- 0
swipe$press_time_diff <- 0
swipe$space_x <- 0.0
swipe$space_y <- 0.0
swipe$velocity_x <- 0.0
swipe$velocity_y <- 0.0

for (id in ids_completed){
  
  id_indices <- which(swipe$user_id == id)
  
  current_action <- -1
  swipe_number <- 0
  
  for (indice in id_indices){
    
    former_indice <- indice -1
    
    # app_action
    if(current_action != swipe[indice]$app_action){
      swipe[indice]$time_diff <- 0
      current_action <- swipe[indice]$app_action
    }
    else{
      swipe[indice]$time_diff <- diff(
        swipe[former_indice:indice]$uptime)
    }
    
    # event_type_pointer
    current_event_type <- swipe[indice]$pointer_event_type
    if (current_event_type == 1){ 
      swipe_number <- swipe_number + 1
      swipe[indice]$press_time_diff <- 0
      swipe[indice]$velocity_x <- 0.0
      swipe[indice]$velocity_y <- 0.0
    }
    else{
      
      swipe[indice]$press_time_diff <- diff(
        swipe[former_indice:indice]$uptime)
      swipe[indice]$space_x <- diff(swipe[former_indice:indice]$x)
      swipe[indice]$space_y <- diff(swipe[former_indice:indice]$y)
      swipe[indice]$velocity_x <- diff(
        (swipe[former_indice:indice]$space_x)/
          swipe[indice]$press_time_diff)
      swipe[indice]$velocity_y <- diff(
        (swipe[former_indice:indice]$space_y)/
          swipe[indice]$press_time_diff)
    }
    swipe[indice]$swipe_number <- swipe_number
  }
}

# without bug action_number

ids_action <- unique(
  swipe[swipe$action_number == 1,]$user_id)

swipe <- swipe %>% 
  filter(user_id %in% ids_action)

# grouped

# user_id, app_action

base <- swipe %>%
  group_by(user_id, app_action)

description_action <- base %>%
  summarise(
    mean_pressure = mean(pressure),
    sd_pressure = sd(pressure),
    cv_pressure = (sd_pressure / mean_pressure) * 100 ,
    mean_pressure = mean(pressure),
    sd_press_time = sd(press_time_diff),
    mean_press_time = mean(press_time_diff),
    cv_press_time = (sd_press_time / mean_press_time) *100,
    obs = n()
  )

# user_id, app_action, action_number

base <- swipe %>%
  group_by(user_id, app_action, action_number)

description_number <- base %>%
  summarise(
    mean_pressure = mean(pressure),
    sd_pressure = sd(pressure),
    cv_pressure = (sd_pressure / mean_pressure) * 100 ,
    mean_pressure = mean(pressure),
    sd_press_time = sd(press_time_diff),
    mean_press_time = mean(press_time_diff),
    cv_press_time = (sd_press_time / mean_press_time) *100,
    obs = n()
  )

base <- swipe %>%
  group_by(app_action)

description <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff)/length(ids_completed),
    mean_pressure = mean(pressure)/length(ids_completed),
    obs = n()/length(ids_completed)
  )

base <- swipe %>%
  group_by(user_id, app_action)

description <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    obs = n()
  )

mean(description[description$app_action == 0,]$mean_press_time)
mean(description[description$app_action == 2,]$mean_press_time)

swipe <- NULL
base <- NULL
description <- NULL


