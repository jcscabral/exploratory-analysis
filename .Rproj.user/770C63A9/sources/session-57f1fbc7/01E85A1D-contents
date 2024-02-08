
library(tidyverse)
library(data.table)
library(ggplot2)

# constants #

SWIPE_HOME <- 0
SWIPE_HOME_BUTTON <- 1
SWIPE_PIX_SEND <- 2 
SWIPE_PIX_SEND_BUTTON <- 3
SWIPE_PIX_RECEIVE_BUTTON <- 4
KEYBOARD_LOGIN <- 5
KEYBOARD_AUTH <- 6
KEYBOARD_PIX_MONEY <- 7
KEYBOARD_PIX_CPF <- 8

path_parent <- '/home/jcscabral/Studies/UspEsalq/pesquisa/20240124/'

# SWIPE #

file <- 'swipeData.csv'  
path_csv <- paste(path_parent,file, sep = '')
swipe <- fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 
            'pressure', 'x', 'y', 'uptime')  
names(swipe) <- cnames

ids_completed <- unique(swipe[swipe$action_number == 7]$user_id)
users_swipe <- swipe %>% filter(user_id %in% ids_completed)

# add swipe_number time_diff

users_swipe$swipe_number <- 0
users_swipe$time_diff <- 0
users_swipe$press_time_diff <- 0
users_swipe$space_x <- 0.0
users_swipe$space_y <- 0.0
users_swipe$velocity_x <- 0.0
users_swipe$velocity_y <- 0.0

for (id in ids_completed){
  
  id_indices <- which(swipe$user_id == id)
  
  current_action <- -1
  swipe_number <- 0
  
  for (indice in id_indices){
    
    former_indice <- indice -1
    
    # app_action
    if(current_action != users_swipe[indice]$app_action){
      users_swipe[indice]$time_diff <- 0
      current_action <- users_swipe[indice]$app_action
    }
    else{
      users_swipe[indice]$time_diff <- diff(
        users_swipe[former_indice:indice]$uptime)
    }
    
    # event_type_pointer
    current_event_type <- users_swipe[indice]$pointer_event_type
    if (current_event_type == 1){ 
      swipe_number <- swipe_number + 1
      users_swipe[indice]$press_time_diff <- 0
      users_swipe[indice]$velocity_x <- 0.0
      users_swipe[indice]$velocity_y <- 0.0
    }
    else{
      
      users_swipe[indice]$press_time_diff <- diff(
        users_swipe[former_indice:indice]$uptime)
      users_swipe[indice]$space_x <- diff(users_swipe[former_indice:indice]$x)
      users_swipe[indice]$space_y <- diff(users_swipe[former_indice:indice]$y)
      users_swipe[indice]$velocity_x <- diff(
        (users_swipe[former_indice:indice]$space_x)/users_swipe[indice]$press_time_diff)
      users_swipe[indice]$velocity_y <- diff(
        (users_swipe[former_indice:indice]$space_y)/users_swipe[indice]$press_time_diff)
    }
    users_swipe[indice]$swipe_number <- swipe_number
  }
  # time_diff #
  
}

diff(users_swipe[241:242]$uptime)

v <-c(1:10)
v[2:3]
# pressure by action_number

swipe_home <- users_swipe[users_swipe$app_action == 0]

base <- swipe_home %>%
  group_by(user_id, action_number)

description <- base %>%
  summarise(
    mean_pressure = mean(pressure),
    obs = n()
  )

# visualize

ggplot(
    data = swipe_home,  
    aes(x= action_number, y = pressure, group = user_id)
  ) +
  geom_point(size = 2.5) +
  geom_line()
  theme_classic()







for (action_number in c(1:7)){
}




    user_data <- data %>% filter(
      users_swipe$user_id == user_id && 
        users_swipe$action_number == action_number
      )






# SENSORS #

file = 'sensorsData.csv'  
path_csv = paste(path_parent,file, sep = '')
sensors  = fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'sensor_type', 'x', 'y', 'z', 'timestamp')  
names(sensors) <- cnames

users_sensors <- sensors %>% filter(user_id %in% ids_completed)


# KEYBOARD #

file <- 'keyboardData.csv'  
path_csv <- paste(path_parent, file, sep = '')
keyboard <- fread(path_csv)
cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
            'character', 'pressure', 'x', 'y', 'uptime')  
names(keyboard) <- cnames
table(keyboard$app_action)
table(keyboard$action_number)

users_keyboard <- keyboard %>% filter(user_id %in% ids_completed)


# first only login or auth #

keyboard_auth <- users_keyboard[
  users_keyboard$app_action == KEYBOARD_LOGIN || 
    users_keyboard$app_action == KEYBOARD_AUTH]

base <- keyboard_auth %>%
  group_by(user_id, action_number, character)

base <- keyboard_auth %>%
  group_by(user_id, character)

description <- base %>%
  summarise(
    mean_pressure = mean(pressure),
    mean_time = mean(uptime),
    obs = n()
  )


