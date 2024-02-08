library(tidyverse)
library(data.table)
library(ggplot2)


swipe_file <- 'swipeData.csv'  
keyboard_file <- 'keyboardData.csv'  
sensors_file <- 'sensorsData.csv'

path_parent <- '/home/jcscabral/Studies/UspEsalq/pesquisa/'

files <- c('20240119', '20240122_20', '20240123', '20240124', 
           '20240125', '20240126')

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
length(ids_completed) #31

keyboard <- keyboard.total %>% filter(user_id %in% ids_completed)
keyboard.total =  NULL

# new variables

keyboard$time_diff <- 0
keyboard$press_time_diff <- 0
keyboard$space_x <- 0.0
keyboard$space_y <- 0.0
keyboard$velocity_x <- 0.0
keyboard$velocity_y <- 0.0

for (id in ids_completed){
  
  id_indices <- which(keyboard$user_id == id)
  
  current_action <- -1
  
  for (indice in id_indices){
    
    former_indice <- indice -1
    
    # app_action
    if(current_action != keyboard[indice]$app_action){
      keyboard[indice]$time_diff <- 0
      current_action <- keyboard[indice]$app_action
    }
    else{
      keyboard[indice]$time_diff <- diff(
        keyboard[former_indice:indice]$uptime)
    }
    
    # event_type_pointer
    current_event_type <- keyboard[indice]$pointer_event_type
    if (current_event_type == 1){ 
      keyboard[indice]$press_time_diff <- 0
      keyboard[indice]$velocity_x <- 0.0
      keyboard[indice]$velocity_y <- 0.0
    }
    else{
      
      keyboard[indice]$press_time_diff <- diff(
        keyboard[former_indice:indice]$uptime)
      keyboard[indice]$space_x <- diff(keyboard[former_indice:indice]$x)
      keyboard[indice]$space_y <- diff(keyboard[former_indice:indice]$y)
      keyboard[indice]$velocity_x <- diff(
        (keyboard[former_indice:indice]$space_x)/
          keyboard[indice]$press_time_diff)
      keyboard[indice]$velocity_y <- diff(
        (keyboard[former_indice:indice]$space_y)/
          keyboard[indice]$press_time_diff)
    }
  }
}

# grouped #

base <- keyboard %>%
  group_by(user_id, app_action, action_number)

description <- base %>%
  summarise(
    mean_press_time = mean(press_time_diff),
    mean_pressure = mean(pressure),
    obs = n()
  )

# without bug of first actions

ids_action <- unique(
  description[description$action_number == 1,]$user_id)

keyboard_login <- keyboard %>% 
  filter((user_id %in% ids_action) &
           app_action == 6)

base <- keyboard_login %>%
  group_by(user_id)

description_login <- base %>%
  summarise(
    median_time = median(press_time_diff),
    mean_time = mean(time_diff),
    sd_time = sd(time_diff),
    median_pressure = median(pressure),
    mean_pressure = mean(pressure),
    sd_pressure = var(pressure),
  )

# coefficient of variation (NRMSD)

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

