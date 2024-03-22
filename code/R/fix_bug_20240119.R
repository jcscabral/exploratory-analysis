#
# day 20230119 #
#


library(tidyverse)
library(data.table)

# ERRORS
# (I)  7, 8 twice
# (II)   all started w/ 2
# (III)  2 logged out
# (IV)  11 started w/ 3


################
### KEYBOARD ###
################

keyboard_data_20240119 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240119/'
  
  SWIPE_HOME <- 0
  SWIPE_HOME_BUTTON <- 1
  SWIPE_PIX_SEND <- 2 
  SWIPE_PIX_SEND_BUTTON <- 3
  SWIPE_PIX_RECEIVE_BUTTON <- 4
  KEYBOARD_LOGIN <- 5
  KEYBOARD_AUTH <- 6
  KEYBOARD_PIX_MONEY <- 7
  KEYBOARD_PIX_CPF <- 8
  
  file <- 'keyboardData.csv'  
  path_csv <- paste(path_parent, file, sep = '')
  keyboard <- fread(path_csv)
  cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 
              'character', 'pressure', 'x', 'y', 'uptime')  
  names(keyboard) <- cnames
  
  ids <- unique(keyboard[keyboard$action_number == 7,]$user_id)
  keyboard <- keyboard %>% filter(user_id %in% ids)
  ids_error1 <- ids[ids !=11 & ids !=2]
  
  
  ###################
  # Fix [error I] #
  ###################
  
  
  keyboard$diff_time <- 0
  
  id <- 7
  
  keyboard[keyboard$user_id == id]$diff_time <-
    c(0, diff(keyboard[keyboard$user_id == id,]$uptime))
  
  indices <- which(keyboard$user_id == 7)
  id_last <- which((keyboard$user_id == id) & 
                     (keyboard$diff_time > 100000))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  keyboard <- keyboard[-indices_to_delete]
  
  
  id <- 8
  
  keyboard[keyboard$user_id == id]$diff_time <-
    c(0, diff(keyboard[keyboard$user_id == id,]$uptime))
  
  indices <- which(keyboard$user_id == id)
  id_last <- which((keyboard$user_id == id) & 
                     (keyboard$diff_time > 100000))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  keyboard <- keyboard[-indices_to_delete]
  
  
  ##############
  # [error II] #
  ##############
  
  
  KEYBOARD_AUTH <- 6
  
  for (id in ids_error1){
    
    id_indices <- which((keyboard$user_id == id) &
                          (keyboard$action_number < 3))
    
    first_index_auth <- match(c(KEYBOARD_AUTH), 
                              keyboard[id_indices]$app_action)
    
    user <- keyboard[id_indices]
    i <- first_index_auth
    while (user$app_action[i] == KEYBOARD_AUTH) {
      i <- i+1
    }
    
    start_indice <- id_indices[1]
    end_indice <- id_indices[i - 1]   
    keyboard[start_indice:end_indice]$action_number <-1
    
  }
  
  
  ###############
  # [error III] #
  ###############
  
  
  KEYBOARD_AUTH <- 6
  
  # fix first action_number 0
  id_indices <- which((keyboard$user_id == 2) &
                        (keyboard$action_number < 3))
  
  first_index_auth <- match(c(KEYBOARD_AUTH),
                            keyboard[id_indices]$app_action)
  
  user = keyboard[id_indices]
  i <- first_index_auth
  while (user$app_action[i] == KEYBOARD_AUTH) {
    i <- i+1
  }
  
  start_indice <- id_indices[1]
  end_indice <- id_indices[i - 1]   
  keyboard[start_indice:end_indice]$action_number <-0
  
  id_indices <- which((keyboard$user_id == 2) &
                        (keyboard$action_number == 2))
  
  
  # fix first action_number 1
  first_index_auth <- match(c(KEYBOARD_AUTH),
                            keyboard[id_indices]$app_action)
  
  user = keyboard[id_indices]
  i <- first_index_auth
  while (user$app_action[i] == KEYBOARD_AUTH) {
    i <- i+1
  }
  
  start_indice <- id_indices[1]
  end_indice <- id_indices[i - 1]   
  keyboard[start_indice:end_indice]$action_number <-1
  
  
  ##############
  # [error IV] #
  ##############
  
  
  previous_action = 3
  id_error2 <- which(keyboard$user_id == 11)
  first_index_auth <- match(previous_action, 
                            keyboard[keyboard$user_id==11]$action_number)
  user <- keyboard[id_error2]
  i = first_index_auth
  while (user$action_number[i] == previous_action) {
    i <- i+1
  }  
  start_indice <- id_error2[1]
  end_indice <- id_error2[i - 1]   
  keyboard[start_indice:end_indice]$action_number <-1
  
  
  keyboard$diff_time <- NULL
  
  return(keyboard)
  
}

# data <- keyboard_data_20240119()
# View(data)
# plot(data[data$user_id == 7,]$x)


# Grouping

# 
# base <- keyboard %>%
#   group_by(user_id, action_number)
# 
# description = base %>%
#   summarise(
#     action_number = mean(action_number),
#     obs = n()
#   )


#############
### SWIPE ###
#############


swipe_data_20240119 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240119/'
  
  SWIPE_HOME <- 0
  SWIPE_HOME_BUTTON <- 1
  SWIPE_PIX_SEND <- 2 
  SWIPE_PIX_SEND_BUTTON <- 3
  SWIPE_PIX_RECEIVE_BUTTON <- 4
  KEYBOARD_LOGIN <- 5
  KEYBOARD_AUTH <- 6
  KEYBOARD_PIX_MONEY <- 7
  KEYBOARD_PIX_CPF <- 8
  
  file = 'swipeData.csv'
  path_csv = paste(path_parent,file, sep = '')
  swipe = fread(path_csv)
  cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type', 
              'pressure', 'x', 'y', 'uptime')  
  names(swipe) <- cnames
  
  ids <- unique(swipe[swipe$action_number == 7,]$user_id)
  ids_error1 <- ids[ids !=11 & ids !=2]
  
  
  #################
  # Fix [error I] #
  #################
  
  
  swipe$diff_time <- 0
  
  id <- 7
  
  swipe[swipe$user_id == id]$diff_time <-
    c(0, diff(swipe[swipe$user_id == id,]$uptime))
  
  max_diff_time <- max(swipe$diff_time)
  
  indices <- which(swipe$user_id == id)
  id_last <- which((swipe$user_id == id) & 
                     (swipe$diff_time == max_diff_time))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  swipe <- swipe[-indices_to_delete]
  
  
  id <- 8
  
  swipe[swipe$user_id == id]$diff_time <-
    c(0, diff(swipe[swipe$user_id == id,]$uptime))
  
  max_diff_time <- max(swipe$diff_time)
  
  indices <- which(swipe$user_id == id)
  id_last <- which((swipe$user_id == id) & 
                     (swipe$diff_time == max_diff_time))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  swipe <- swipe[-indices_to_delete]
  
  
  ##################
  # Fix [error II] #
  ##################
  
  
  for (id in ids_error1){
    
    id_indices <- which((swipe$user_id == id) &
                          (swipe$action_number < 3))
    
    first_index_pix <- match(c(SWIPE_PIX_SEND), 
                             swipe[id_indices]$app_action)
    
    user = swipe[id_indices]
    i = first_index_pix
    while (user$app_action[i] == SWIPE_PIX_SEND) {
      i <- i+1
    }
    start_indice <- id_indices[1]
    end_indice <- id_indices[i - 1]   
    swipe[start_indice:end_indice]$action_number <-1
  }
  
  
  ###############
  # [error III] #
  ###############
  
  
  id_indices <- which((swipe$user_id == 2) &
                        (swipe$action_number < 3))
  
  first_index_pix <- match(c(SWIPE_PIX_SEND),
                           swipe[id_indices]$app_action)
  
  user = swipe[id_indices]
  i <- first_index_pix
  while (user$app_action[i] == SWIPE_PIX_SEND) {
    i <- i+1
  }
  
  start_indice <- id_indices[1]
  end_indice <- id_indices[i - 1]   
  swipe[start_indice:end_indice]$action_number <-0
  
  id_indices <- which((swipe$user_id == 2) &
                        (swipe$action_number == 2))
  
  
  # fix first action_number 1
  first_index_pix <- match(c(SWIPE_PIX_SEND),
                           swipe[id_indices]$app_action)
  
  user = swipe[id_indices]
  i <- first_index_pix
  while (user$app_action[i] == SWIPE_PIX_SEND) {
    i <- i+1
  }
  
  start_indice <- id_indices[1]
  end_indice <- id_indices[i - 1]   
  swipe[start_indice:end_indice]$action_number <-1
  
  
  ##############
  # [error IV] #
  ##############
  
  id <- 11
  
  indices_error4 <- which(swipe$user_id == id)
  user <- swipe[indices_error4]
  id_action2 <- match(c(2), user$action_number)
  
  start_indice <- indices_error4[1]
  end_indice <- indices_error4[id_action2] -1
  
  swipe[start_indice:(end_indice)]$action_number <- 1
 
  swipe$diff_time <- NULL
  return(swipe) 
  
}

# data <- swipe_data_20240119()
# View(data)
# plot(data[data$user_id == 7,]$app_action)



###############
### SENSORS ###
###############



sensor_data_20240119 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240119/'
  
  SWIPE_HOME <- 0
  SWIPE_HOME_BUTTON <- 1
  SWIPE_PIX_SEND <- 2 
  SWIPE_PIX_SEND_BUTTON <- 3
  SWIPE_PIX_RECEIVE_BUTTON <- 4
  KEYBOARD_LOGIN <- 5
  KEYBOARD_AUTH <- 6
  KEYBOARD_PIX_MONEY <- 7
  KEYBOARD_PIX_CPF <- 8
  
  file = 'sensorsData.csv'  
  path_csv = paste(path_parent,file, sep = '')
  sensors  = fread(path_csv)
  cnames <- c('user_id', 'action_number', 'app_action', 
              'sensor_type', 'x', 'y', 'z', 'timestamp')  
  names(sensors) <- cnames
  sensors$timestamp <- sensors$timestamp / 1000000 # nano to milliseconds
  
  
  
  ###############
  # [error III] #
  ###############
  
  ids <- unique(sensors[sensors$action_number == 7,]$user_id)
  ids_error1 <- ids[ids !=11 & ids !=2]
  
  id <- 2
  
  # original indexes
  id_indices <- which((sensors$user_id == id) &
                        (sensors$action_number < 3))
  
  # action_number 0
  
  first_index_pix <- match(c(SWIPE_PIX_SEND), 
                           sensors[id_indices]$app_action)
  
  start_indice <- id_indices[first_index_pix]
  end_indice <- max(id_indices)
  
  second_index_home <- match(c(SWIPE_HOME),
                             sensors[start_indice:end_indice]$app_action)
  
  end_index <- first_index_pix + second_index_home -1
  
  start_indice <- min(id_indices)
  end_indice <- id_indices[end_index -1]   
  
  sensors[start_indice:end_indice]$action_number <-0
  
  
  # action_number 1
  
  
  start_indice <- end_indice + 1
  end_indice <- max(id_indices)
  
  second_index_pix <- match(c(SWIPE_PIX_SEND),
                            sensors[start_indice:end_indice]$app_action)
  
  start_indice <- id_indices[(end_index -1 + second_index_pix)]
  end_indice <- max(id_indices)
  
  third_index_home <- match(c(SWIPE_HOME),
                            sensors[start_indice:end_indice]$app_action)
  
  last_index <- end_index + second_index_pix -1 + third_index_home -1
  
  start_indice <- id_indices[end_index]
  end_indice <- id_indices[last_index -1 ]
  
  sensors[start_indice:end_indice]$action_number <-1
  
  
  
  #############
  # [error 0] #
  #############
  
  
  
  # correct order: 0, 2, 7, 8, 6
  


  indices_to_delete <- c()
  
  is_keyboards <- F
  previous_action_number <- 0
  is_home <- F
  is_pix <- F
  is_keyboard_money <- F
  is_keyboard_cpf <- F
  is_keyboard_auth <- F
  
  for (id in ids){
    
    id_indices <- which(sensors$user_id == id)
    
    for (i in c(1:length(id_indices))){
      
      row <- sensors[id_indices[i]]
      app_action <- row$app_action
      action_number <- row$action_number  
      
      if(action_number != previous_action_number){
        is_home <- T
        is_pix <- F
        is_keyboards <- F
        is_keyboard_cpf <- F
        is_keyboard_auth <- F
      }
      
      if(app_action == SWIPE_HOME){
        is_keyboards <- F
        is_home <- T
        is_keyboard_auth <- F
      }
      if(app_action == SWIPE_PIX_SEND &&
         is_home == T){
        is_keyboards <- F
        is_pix <- T
        is_home <- F
      }
      # last possible action not keyboards.
      # keyboards restarts here
      if(is_pix == T &&
         app_action == KEYBOARD_PIX_MONEY){
        is_keyboards <- T
      }
      
      # between no critical keyboard data
      if(is_keyboards == F){
        if(app_action == KEYBOARD_PIX_MONEY ||
           app_action == KEYBOARD_AUTH ||
           app_action == KEYBOARD_PIX_CPF){
          indices_to_delete <- append(
            indices_to_delete, id_indices[i])
        }
      } else {
        
        # setting current keyboard action
        if(app_action == KEYBOARD_PIX_MONEY &&
           is_keyboard_cpf == F &&
           is_keyboard_auth == F){
          is_keyboard_money <- T
        }
        
        if(app_action == KEYBOARD_PIX_CPF &&
           is_keyboard_money == T &&
           is_keyboard_auth == F
        ){
          is_keyboard_money <- F
          is_keyboard_cpf <- T
        }
        if(app_action == KEYBOARD_AUTH &&
           is_keyboard_cpf == T){
          is_keyboard_auth <- T
          is_keyboard_cpf <- F
        }
        
      }
      previous_action_number <- action_number
    }
    
  }  
  
  sensors <- sensors[-indices_to_delete]
  
  
  #############
  # [error I] #
  #############
  
  
  
  sensors$diff_time <- 0
  
  id <- 7
  
  sensors[sensors$user_id == id]$diff_time <-
    c(0, diff(sensors[sensors$user_id == id,]$timestamp))
  
  max_diff_time <- max(sensors[sensors$user_id == id]$diff_time)
  
  indices <- which(sensors$user_id == id)
  id_last <- which((sensors$user_id == id) & 
                     (sensors$diff_time == max_diff_time))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  sensors <- sensors[-indices_to_delete]
  
  
  id <- 8
  
  sensors[sensors$user_id == id]$diff_time <-
    c(0, diff(sensors[sensors$user_id == id,]$timestamp))
  
  max_diff_time <- max(sensors[sensors$user_id == id]$diff_time)
  
  indices <- which(sensors$user_id == id)
  id_last <- which((sensors$user_id == id) & 
                     (sensors$diff_time == max_diff_time))
  
  indices_to_delete <- c((id_last + 1): max(indices))
  sensors <- sensors[-indices_to_delete]
  
  
  
  ##################
  # Fix [error II] #
  ##################
  
  
  for (id in ids_error1){
    
    id_indices <- which((sensors$user_id == id) &
                          (sensors$action_number < 3))
    
    first_index_pix <- match(c(SWIPE_PIX_SEND), 
                              sensors[id_indices]$app_action)
    
    second_index_home <- match(c(SWIPE_HOME),
      sensors[id_indices[first_index_pix]:max(id_indices)]$app_action)
    
    start_indice <- id_indices[1]
    end_indice <- id_indices[first_index_pix -1 + second_index_home - 1]   
    sensors[start_indice:end_indice]$action_number <-1
  }
  
  
  
  ############################
  # [error IV]
  # case user_id == 11
  # action_number starts w/ 3
  ############################
  
  
  
  id <- 11
  
  indices_error4 <- which(sensors$user_id == id)
  user <- sensors[indices_error4]
  id_action2 <- match(c(2), user$action_number)
  
  start_indice <- indices_error4[1]
  end_indice <- indices_error4[id_action2] -1
  sensors[start_indice:(end_indice)]$action_number <- 1
  
  sensors$diff_time <- NULL  
  
  return(sensors)
  
}

# data <- sensor_data_20240119()
# View(data)
# plot(data[data$user_id == 1,]$app_action)



# sensors[order(
#   sensors$user_id, 
#   sensors$action_number, 
#   sensors$app_action),]

# base <- sensors %>%
#   group_by(
#     user_id, 
#     action_number,
#     app_action)
# 
# description = base %>%
#   summarise(
#     action_number = mean(action_number),
#     obs = n()
#   )




