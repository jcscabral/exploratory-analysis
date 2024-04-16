
# Reference: (Buriro, 2017)

USE_SENSOR_DATA <-T

############
# KEYBOARD #
############


loadKeyboard <- function(loadFixed = F) {

  # Load ####

  keyboard.total = NULL
  
  for(file in FILES_B){
    
    path_day <- paste(PATH_PARENT, file, '/', sep = '')
    
    full_path <- paste(path_day, KEYBOARD_FILE, sep = '')
    keyboard <- fread(full_path)
    cnames <- c('user_id', 'action_number', 'app_action', 'pointer_event_type',
                'character', 'pressure', 'x', 'y', 'uptime')  
    names(keyboard) <- cnames
    
    ids_completed <- unique(keyboard[keyboard$action_number == 7]$user_id)
    keyboard.completed <- keyboard %>% filter(user_id %in% ids_completed)
    
    # it started with user_id == 51
    # if (file == '20240228'){
    #   keyboard[keyboard$user_id>50]
    # }
    if(is.null(keyboard.total)){
      keyboard.total <- keyboard.completed
    }
    else{
      keyboard.total <-rbind(keyboard.total, keyboard.completed)
    }
  }
  
  keyboard <- NULL
  keyboard.completed <- NULL

  keyboard <- keyboard.total #%>% filter(user_id %in% ids_completed)
  keyboard.total =  NULL
  
  if (loadFixed == T){
    # fixed data
    keyboard <- rbind(keyboard, keyboard_data_20240119())
    keyboard <- rbind(keyboard, keyboard_data_20240122_20())
  }
  
  ids_completed <- unique(keyboard$user_id)
  
  # end load ####
  
  # new variables ####
  
  keyboard$time_diff <- 0
  keyboard$press_time_diff <- 0
  keyboard$pressure_var <- 0.0 
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
  
  # end new ####
  
  return(keyboard)

}

keyboard <- loadKeyboard(F)
ids_completed <- sort(unique(keyboard$user_id))

# plot #######

keyboard.login <- keyboard %>% filter(
  app_action %in% c(KEYBOARD_LOGIN))

keyboard.user <- keyboard.login[(keyboard.login$user_id == 74)]

p0 <- ggplot(keyboard.user, 
  aes(uptime, pressure)) + geom_point()

keyboard.user <- NULL
keyboard.login <- NULL

# end plot####

keyboard_login <- keyboard %>%
  filter(app_action == 5 | app_action == 6)

##########
# SENSOR #
##########

if (USE_SENSOR_DATA == T){
  sensors.login <- loadSensorsLogin()
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

###############
# STATS CARDS #
###############

# stats functions ####

keyboardStats <- function(dfdata, digit){
  card_stats <- dfdata %>%
    filter(character == digit) %>%
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
      ft_qm = qm(fly_time) ,
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
      
      # # Pressure var
      # y_md = median(pressure_var, na.rm = T),
      # y_fq = qt(pressure_var, 1),
      # y_tq = qt(pressure_var, 3),
      # y_ir = IQR(pressure_var, na.rm = T),
      # y_am = mean(pressure_var, na.rm = T),
      # y_vr = var(pressure_var),
      # y_sd = sd(pressure_var, na.rm = T),
      # y_cv = cv(pressure_var, na.rm = T),
      # y_se = se(pressure_var),
      # y_mn = min(pressure_var, na.rm = T) ,
      # y_mx = max(pressure_var, na.rm = T) ,
      # y_qm = qm(pressure_var),
      # y_rg = rg(pressure_var) ,
      # y_sk = skewness(pressure_var) ,    
      # y_ku = kurtosis(pressure_var)
    )
  return(card_stats)
}

# end card stats ####

# Renaming chars ####
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
# end ####

# data grouped by param
getCharStats <- function(dbase, secao = T){
  
  groupby <- c("user_id")
  ini.range <- 3
  if (secao == T){
    groupby <- c("user_id","action_number")
    ini.range <- 4
  }
  desc_login_char_5 <- keyboardStats(dbase, 5)
  desc_login_char_4 <- keyboardStats(dbase, 4)
  desc_login_char_8 <- keyboardStats(dbase, 8)
  desc_login_char_3 <- keyboardStats(dbase, 3)
  desc_login_char_1 <- keyboardStats(dbase, 1)
  desc_login_char_7 <- keyboardStats(dbase, 7)
  desc_login_char_9 <- keyboardStats(dbase, 9)
  desc_login_char_0 <- keyboardStats(dbase, 0)
  
  col.names <- names(desc_login_char_5)
  col.names <- col.names[ini.range:length(col.names)]
  
  desc_login_char_5 <- desc_login_char_5 %>% 
    rename_with(.fn = col_char5, .cols = col.names)
  desc_login_char_4 <- desc_login_char_4 %>% 
    rename_with(.fn = col_char4, .cols = col.names)
  desc_login_char_8 <- desc_login_char_8 %>% 
    rename_with(.fn = col_char8, .cols = col.names)
  desc_login_char_3 <- desc_login_char_3 %>% 
    rename_with(.fn = col_char3, .cols = col.names)
  desc_login_char_1 <- desc_login_char_1 %>% 
    rename_with(.fn = col_char1, .cols = col.names)
  desc_login_char_7 <- desc_login_char_7 %>% 
    rename_with(.fn = col_char7, .cols = col.names)
  desc_login_char_9 <- desc_login_char_9 %>% 
    rename_with(.fn = col_char9, .cols = col.names)
  desc_login_char_0 <- desc_login_char_0 %>% 
    rename_with(.fn = col_char0, .cols = col.names)
  
  desc_login_chars <- inner_join(
    desc_login_char_5, desc_login_char_4, by = groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_8, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_3, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_1, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_7, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_9, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  desc_login_chars <- inner_join(
    desc_login_chars, desc_login_char_0, by= groupby)
  desc_login_chars$character.x <- NULL
  desc_login_chars$character.y <- NULL
  
  return(desc_login_chars)
  
}

infoGainCols <- function(dbase){
  
  cols.not.na <- names(which(colSums(is.na(dbase)) == 0))
  dbase <- dbase[cols.not.na]
  
  threshold <- length(ids_completed)/ (ncol(dbase) - 1)
  result <- FSelectorRcpp::information_gain(
    formula = user_id ~ .,
    data = dbase ,
    type = 'infogain'
  )
  cols_ranked  <- result %>% 
    filter(importance >= threshold) %>% select(attributes)
  cols_perc  <- result %>% 
    filter(importance >= threshold) %>% select(importance)
  #return(as_vector(cols_ranked))
  return(
     list(
      "cols_ranked" = as_vector(cols_ranked),
      "cols_perc" = as_vector(cols_perc)
    )
  )
}

# CROSS VALIDATION #####

df.results <- data.frame(
  
  cutoff = double(),
  pc_train = double(),
  iter = integer(),
  num_users = integer(),
  num_attackers = integer(),
  num_variables = integer(),
  login_pos = integer(),
  login_neg = integer(),
  login_pos_prob = integer(),
  login_neg_prob = integer(),
  attack_pos = integer(),
  attack_neg = integer(),
  grd_login_pos = integer(),
  grd_login_neg = integer(),
  grd_login_pos_prob = integer(),
  grd_login_neg_prob = integer(),
  grd_attack_pos = integer(),
  grd_attack_neg = integer()
)

df.variables <- data.frame(
  iter = integer(),
  no_var = character(),
  pc_var = double()
)


# I. set sessions 

NUM_SESSIONS <- 5

# II - split data: test with only real unknown intruder 

ids <- sample(ids_completed)

PC_30 <- 0.3
PC_50 <- 0.5
PC_70 <- 0.7
PC_90 <- 0.9
CUTOFFS <- c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5) 
percents <-c(PC_30, PC_50, PC_70, PC_90) 

iters <- c(0,1)
row <- 1
for (pc_train in percents){
  
  # iter
  # 0: train/test:[1-5], val:6 
  # 1: train/test:[1-5], val:7
  
  for (iter in iters){
    
    # split train and attack's validation
    session_start <- 1 + iter
    session_end <- NUM_SESSIONS + iter
    
    ind <- round(length(ids) * pc_train)
    ids.train <-ids[1:ind]
    ids.val <-ids[(ind+1):length(ids)]
    
    num_users <- length(ids.train)
    num_attackers <- length(ids.val)
    
    
    ##########################
    # I- template (training) #
    ##########################
    
    keyboard.train <- keyboard_login %>% 
      filter(user_id %in% ids.train)
    
    
    if (USE_SENSOR_DATA == T){
      sensors.train <- sensors.login  %>% 
        filter(user_id %in% ids.train)
    }
    
    keyboard.grouped <- keyboard.train %>%
      filter(action_number >= session_start,
             action_number <= session_end) %>%
      group_by(user_id, character, action_number)
    
    keyboard.stats <- getCharStats(keyboard.grouped)
    #info.keyboard.cols <- infoGainCols(keyboard.stats)
    info.gaing <- infoGainCols(keyboard.stats)
    info.keyboard.cols <- info.gaing["cols_ranked"]$cols_ranked
    info.keyboard.perc <- info.gaing["cols_perc"]$cols_perc
    
    keyboard.stats <- keyboard.stats[c("user_id", info.keyboard.cols)]
    
    info.cols <- info.keyboard.cols
    info.perc <- info.keyboard.perc
    
    info.sensor.cols = NULL
    if (USE_SENSOR_DATA == T){
      
      sensors.train.grouped <- sensors.train %>%
        filter(action_number >= session_start,
               action_number <= session_end) %>%
        group_by(user_id, action_number)
      
      sensors.stats <- sensorStats(sensors.train.grouped)
      #info.sensor.cols <- infoGainCols(sensors.stats)
      info.gaing <- infoGainCols(sensors.stats)
      info.sensor.cols <- info.gaing["cols_ranked"]$cols_ranked
      info.sensor.perc <- info.gaing["cols_perc"]$cols_perc
      
      sensors.stats <- sensors.stats[info.sensor.cols]
      keyboard.stats <- bind_cols(keyboard.stats, sensors.stats)
      
      info.cols <- c(info.cols, info.sensor.cols)
      info.perc <-c(info.perc, info.sensor.perc)
      
    }
    
    df.variables <- rbind(
      df.variables,
      data.frame(
        iter = row, 
        no_var = info.cols,
        pc_var = info.perc)
    )
    
    card.train <- keyboard.stats
    num_variables <- length(info.cols) -1
    
    
    ##############
    # II - login #
    ##############
    
    keyboard.login <- keyboard.train %>%
      filter(action_number == session_end + 1) %>%
        group_by(user_id, character, action_number)
    keyboard.login.stats <- getCharStats(keyboard.login)
    keyboard.login.stats <- keyboard.login.stats[
      c("user_id", info.keyboard.cols)]
    
    if (USE_SENSOR_DATA == T){
      sensor.login.grouped <- sensors.train %>%
        filter(action_number == session_end + 1) %>%
        group_by(user_id, action_number)
      
      sensors.login.stats <- sensorStats(sensor.login.grouped)
      sensors.login.stats <- sensors.login.stats[info.sensor.cols]
      
      keyboard.login.stats <- bind_cols(keyboard.login.stats, 
          sensors.login.stats)
    }
    card.login <- keyboard.login.stats
    
    
    ####################
    # III - validation #
    ####################
    
    keyboard.val <- keyboard_login %>% 
      filter(user_id %in% ids.val)
    
    if (USE_SENSOR_DATA == T){
      sensors.val <- sensors.login %>% 
        filter(user_id %in% ids.val)
    }
    
    keyboard.val.grouped <- keyboard.val %>%
       group_by(user_id, character, action_number)
    keyboard.val.stats <- getCharStats(keyboard.val.grouped, secao = T)
    keyboard.val.stats <- keyboard.val.stats[c("user_id", info.keyboard.cols)]
    
    if (USE_SENSOR_DATA == T){
      sensor.val.grouped <- sensors.val %>% group_by(user_id, action_number)
      sensors.val.stats <- sensorStats(sensor.val.grouped)
      sensors.val.stats <- sensors.val.stats[info.sensor.cols]
      keyboard.val.stats <- bind_cols(keyboard.val.stats, sensors.val.stats)
    }
    
    card.val <- keyboard.val.stats
    
    ###############
    ### TRAIN #####
    ###############
    
    x_train <- card.train[,-1]
    y_train <- as.factor(card.train$user_id)
    
    x_login <- card.login[,-1]
    y_login <- as.factor(card.login$user_id)
    
    x_val <- card.val[,-1]
    y_val <- as.factor(card.val$user_id)
    
    # randomForest class w/ 5 sections
    rf <- randomForest(
        x_train, 
        y_train, 
        importance = T)
    
    # with k-fold
    df.train <- card.train
    df.train$user_id <- paste("u", df.train$user_id, sep = "")
    control <- trainControl(method = "repeatedcv", 
                            number = 10 ,
                            repeats = 2,
                            search = "grid",
                            classProbs = T)
    
    gridsearch <- train(
      user_id ~ .,
      data = df.train ,
      method = 'rf',
      ntree = 100,
      trControl = control
    )

    for (cutoff in CUTOFFS){
      
      # I. login try
      
      pred.class.login <- predict(object = rf, x_login, type ="class")
      login_pos <- sum(pred.class.login == y_login)
      login_neg <- sum(pred.class.login != y_login)
      
      pred.prob.login <- predict(object = rf, x_login, type ="prob")
      max.probs.login <- apply(pred.prob.login, 1, max)
      matched <- pred.class.login == y_login
      login_pos_prob <- sum(matched[max.probs.login> cutoff])
      login_neg_prob <- length(pred.class.login) - login_pos_prob
      
      
      # II. attack try
      pred.prob.val <- predict(object = rf, x_val, type ="prob")
      max.probs <- apply(pred.prob.val, 1, max, na.rm=TRUE)
      
      attack_pos <- sum(max.probs > cutoff)
      attack_neg <- length(max.probs) - attack_pos
      
        
      # II. gridsearch
      
      # login
      
      df.login.y <- paste("u", y_login, sep = "")
      
      pred.class.kfold <- predict(gridsearch, x_login)
      grd_login_pos <- sum(pred.class.kfold == df.login.y)
      grd_login_neg <- sum(pred.class.kfold != df.login.y)
      grd_login_acc <- grd_login_pos / length(df.login.y)
      
      pred.prob.kfold <- predict(gridsearch, x_login, type = 'prob')
      max.probs.login.kfold <- apply(pred.prob.kfold, 1, max, na.rm=TRUE)
      matched.kfold <- pred.class.kfold == df.login.y
      
      grd_login_pos_prob <- sum(matched.kfold[max.probs.login.kfold> cutoff])
      grd_login_neg_prob <- length(df.login.y) - grd_login_pos_prob
      
      # attack 
      
      pred.prob.attack.kfold <- predict(gridsearch, x_val, type = 'prob')
      max.probs.kfold <- apply(pred.prob.attack.kfold, 1, max, na.rm=TRUE) # max by row
      
      grd_attack_pos <- sum(max.probs.kfold > cutoff)
      grd_attack_neg <- length(max.probs.kfold) - grd_attack_pos
      
      
      df.results[row,] <- c(cutoff,
                            pc_train, 
                            iter, 
                            num_users,
                            num_attackers,
                            num_variables,
                            login_pos,
                            login_neg,
                            login_pos_prob,
                            login_neg_prob,
                            attack_pos,
                            attack_neg,
                            grd_login_pos,
                            grd_login_neg,
                            grd_login_pos_prob,
                            grd_login_neg_prob,
                            grd_attack_pos,
                            grd_attack_neg
      )
      row <- row + 1
    }
  }
}

write.csv(df.results,'result32-key-sens')

write.csv(df.variables,'variables52-key')