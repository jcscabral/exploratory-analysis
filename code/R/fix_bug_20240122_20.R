
library(tidyverse)
library(data.table)


# error 1: except [5] 19, 8[25]
# error 2: [6] 20

# i = 6
# plot(keyboard[keyboard$user_id == ids[i],]$action_number)
# i = 8
# View(keyboard[keyboard$user_id == ids[i],])

################
### KEYBOARD ###
################

keyboard_data_20240122_20 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240122_20/'
  
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
  ids_error1 <- ids[ids != 19 & ids != 20 & ids != 25]
  
  #############
  # [error I] #
  #############
  
  
  
  for (id in ids_error1){
    
    id_indices <- which((keyboard$user_id == id) &
                          (keyboard$action_number < 3))
    
    # first auth
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
  
  
  
  ##############
  # [error II] #
  ##############
  
  
  
  user_id_error <- 20
  previous_action = 3
  id_error2 <- which(keyboard$user_id == user_id_error)
  first_index_auth <- match(previous_action, 
                            keyboard[keyboard$user_id==user_id_error]$action_number)
  user <- keyboard[id_error2]
  i = first_index_auth
  while (user$action_number[i] == previous_action) {
    i <- i+1
  }  
  start_indice <- id_error2[1]
  end_indice <- id_error2[i - 1]   
  keyboard[start_indice:end_indice]$action_number <-1
  
  return(keyboard)
  
}


#############
### SWIPE ###
#############

# i = 8
# plot(swipe[swipe$user_id == ids[i],]$action_number)
# i = 8
# View(swipe[swipe$user_id == ids[i],])

swipe_data_20240122_20 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240122_20/'
  
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
  swipe$timestamp <- swipe$timestamp / 1000000 # nano to milliseconds
  ids <- unique(swipe[swipe$action_number == 7,]$user_id)
  swipe <- swipe %>% filter(user_id %in% ids)
  ids_error1 <- ids[ids != 19 & ids != 20 & ids != 25]
  
  #################
  # Fix [error I] #
  #################
  
  
  
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
  
  
  
  ##############
  # [error IV] #
  ##############
  
  
  
  id <- 20
  
  indices_error4 <- which(swipe$user_id == id)
  user <- swipe[indices_error4]
  id_action2 <- match(c(2), user$action_number)
  
  start_indice <- indices_error4[1]
  end_indice <- indices_error4[id_action2] -1
  
  swipe[start_indice:(end_indice)]$action_number <- 1
  swipe$timestamp <- NULL
  
  return(swipe) 
  
}




###############
### SENSORS ###
###############

sensor_data_20240122_20 <- function(){
  
  path_parent <- '~/Studies/UspEsalq/Tcc/projeto/pesquisa/datasets/20240122_20/'
  
  SWIPE_HOME <- 0
  SWIPE_HOME_BUTTON <- 1
  SWIPE_PIX_SEND <- 2 
  SWIPE_PIX_SEND_BUTTON <- 3
  SWIPE_PIX_RECEIVE_BUTTON <- 4
  KEYBOARD_LOGIN <- 5
  KEYBOARD_AUTH <- 6
  KEYBOARD_PIX_MONEY <- 7
  KEYBOARD_PIX_CPF <- 8
  
  USER_UNKOWN_ERROR <-24
  
  file <- 'sensorsData.csv'  
  path_csv <- paste(path_parent,file, sep = '')
  sensors <- fread(path_csv)
  cnames <- c('user_id', 'action_number', 'app_action', 
              'sensor_type', 'x', 'y', 'z', 'timestamp')  
  names(sensors) <- cnames
  sensors$timestamp <- sensors$timestamp / 1000000 # nano to milliseconds
  sensors <- sensors[sensors$user_id != USER_UNKOWN_ERROR]
  ids <- unique(sensors[sensors$action_number == 7,]$user_id)
  ids <- ids[ids!=USER_UNKOWN_ERROR]
  
  
  ids_error1 <- ids[ids != 19 & ids != 20 & ids != 25]
  
  
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
  
  
  #################
  # Fix [error I] #
  #################
  
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
  
  
  
  ##############
  # [error II] #
  ##############
  
  
  
  id <- 20
  
  indices_error2 <- which(sensors$user_id == id)
  user <- sensors[indices_error2]
  id_action2 <- match(c(2), user$action_number)
  
  start_indice <- indices_error2[1]
  end_indice <- indices_error2[id_action2] -1
  sensors[start_indice:(end_indice)]$action_number <- 1
  
  return(sensors)
  
}  
